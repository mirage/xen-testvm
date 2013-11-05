(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* TODO: automatically resize the image based on the kernel size *)

module IntMap = Map.Make(struct
  type t = int
  let compare (x: int) (y: int) = compare x y
end)

let upload ~pool ~username ~password ~kernel =
  (* sector number -> disk data *)
  let map = ref IntMap.empty
  (* simple ramdisk to contain the partition table, filesystem,
     kernel and grub config *)
  let module Mem = struct
    type 'a t = 'a
    let ( >>= ) x f = f x
    let return x = x

    let write_sector buf n =
      let sector = Cstruct.create 512 in
      Cstruct.blit buf 0 sector 0 512;
      map := IntMap.add n sector !map

    let read_sector buf n =
      if IntMap.mem n !map
      then Cstruct.blit (IntMap.find n !map) 0 buf 0 512
      else
        for i = 0 to 511 do
          Cstruct.set_uint8 buf i 0
        done
  end in

  Lwt_unix.LargeFile.stat kernel >>= fun stats ->
  if stats.Lwt_unix.LargeFile.st_size > Int64.(mul (mul 14L 1024L) 1024L)
  then failwith "We only support kernels < 14MiB in size";
  let disk_length_bytes = Int32.(mul (mul 16l 1024l) 1024l) in
  let disk_length_sectors = Int32.(div disk_length_bytes 512l) in

  let start_sector = 2048l in
  let length_sectors = Int32.sub disk_length_sectors start_sector in
  let length_bytes = Int32.(mul length_sectors 512l) in
  let partition = Mbr.Partition.make ~active:true ~ty:6 start_sector length_sectors in
  let mbr = Mbr.make [ partition ] in

  let open Fat in
  let open Fat_lwt in
  let open S in
  let ok = function
    | Result.Ok x -> x
    | Result.Error error -> failwith (Error.to_string error) in
  let module MemFS = Fs.Make(Mem) in
  let fs = MemFS.make (Int64.of_int32 length_bytes) in
  let kernel_path = Path.of_string "/kernel" in
  let menu_lst_list = [ "boot"; "grub"; "menu.lst" ] in
  let menu_lst_path = Path.of_string_list menu_lst_list in
  (* mkdir -p *)
  let (_ : string) = List.fold_left (fun dir x ->
    let x' = Filename.concat dir x in
    ok (MemFS.mkdir fs (Path.of_string x'));
    x'
  ) "/" (List.(rev (tl (rev menu_lst_list)))) in
  ok (MemFS.create fs menu_lst_path);

  let menu_lst_file = MemFS.file_of_path fs menu_lst_path in
  let menu_lst_string = String.concat "\n" [
    "default 0";
    "timeout 1";
    "title Mirage";
    "root (hd0,0)";
    "kernel /kernel";
  ] in
  let menu_lst_cstruct = Cstruct.create (String.length menu_lst_string) in
  Cstruct.blit_from_string menu_lst_string 0 menu_lst_cstruct 0 (Cstruct.len menu_lst_cstruct);
  ok (MemFS.write fs menu_lst_file 0 menu_lst_cstruct);

  (* Load the kernel image (into RAM) *)
  ok (MemFS.create fs kernel_path);
  let len = Int64.to_int stats.Unix.LargeFile.st_size in
  Lwt_unix.openfile kernel [ Unix.O_RDONLY ] 0 >>= fun fd ->
  let sector = Cstruct.create (2 * 1024 * 1024) in
  let rec loop offset remaining =
    let this = min remaining (Cstruct.len buffer) in
    let block = Cstruct.sub buffer 0 this in
    really_read fd block >>= fun () ->
    ok (MemFS.write fs kernel_file offset block);
    let offset = offset + this in
    let remaining = remaining - this in
    if remaining > 0
    then loop offset remaining
    else return () in
  loop 0 len >>= fun () ->
  Lwt_unix.close fd >>= fun () ->

  (* Talk to xapi and create the target VDI *)
  let open Xen_api in
  let open Xen_api_lwt_unix in
  let rpc = make uri in
    lwt session_id = Session.login_with_password rpc username password "1.0" in
    try_lwt
      lwt pools = Pool.get_all rpc session_id in
      let pool = List.hd pools in
      lwt sr = Pool.get_default_SR rpc session_id pool in
      lwt vdi = VDI.create ~rpc ~session_id ~name_label:"upload_disk" ~name_description:""
        ~sR:sr ~virtual_size ~_type:`user ~sharable:false ~read_only:false
        ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
      (try_lwt
        let authentication = Disk.UserPassword(!username, !password) in
        let uri = Disk.uri ~pool:(Uri.of_string !uri) ~authentication ~vdi in
        lwt oc = Disk.start_upload ~chunked:false ~uri in

        (* MBR at sector 0 *)
        let sector = Cstruct.create 512 in
        Mbr.marshal sector mbr;
        oc.Data_channel.really_write sector >>= fun () ->
        (* Create an empty sector (upload isn't sparse) *)
        let zeroes = Cstruct.create 512 in
        for i = 0 to Csturct.len zeroes - 1 do
          Cstruct.set_uint8 zeroes i 0
        done;
        let write_zeroes n =
          if n = 0
          then return ()
          else
            oc.Data_channel.really_write zeroes >>= fun () ->
            write_zeroes (n - 1) in
        (* Write the empty blocks before the first partition *)
        write_zeroes (Int32.to_int start_sector - 1) >>= fun () ->
        (* Write each disk block *)
        let rec loop last remaining =
          if IntMap.is_empty remaining
          then return ()
          else begin
            let n, data = IntMap.min_binding remaining in
            write_zeroes (n - last - 1) >>= fun () ->
            oc.Data_channel.really_write data >>= fun () ->
            let remaining = IntMap.remove n remaining in
            loop n remaining
          end in
        loop (-1) !map >>= fun () ->
        oc.Data_channel.close ()
      with e ->
        Printf.fprintf stderr "Caught: %s, cleaning up\n%!" (Printexc.to_string e);
        VDI.destroy rpc session_id vdi >>= fun () ->
        fail e) >>= fun () ->
      return vdi
    finally
      Session.logout rpc session_id

