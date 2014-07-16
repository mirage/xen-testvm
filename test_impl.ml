(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open OS

type context = Blkfront.t list
type 'a t = 'a Lwt.t
let bind = Lwt.bind
let return = Lwt.return
let fail = Lwt.fail
let handle_failure = Lwt.catch

let (>>=) = bind

let shutdown context () = 
  let thread = 
    lwt () = Time.sleep 0.1 in 
    Sched.shutdown Sched.Poweroff;
    Lwt.return ()
  in
  Lwt.return ()

let reboot context () =
  let thread = 
    lwt () = Time.sleep 0.1 in
    Sched.shutdown Sched.Reboot;
    Lwt.return () 
  in
  Lwt.return ()

let crash context () =
  let thread = 
    lwt () = Time.sleep 0.1 in
    Sched.shutdown Sched.Reboot;
    Lwt.return ()
  in
  Lwt.return ()

module Vbd = struct
  let write_sector context devid sector contents = 
    let contents = Cohttp.Base64.decode contents in
    (* Write page! *)
    lwt () =
      if String.length contents <> 4096 
      then Lwt.fail (Failure (Printf.sprintf "Expecting 4096 bytes of contents (got %d)" (String.length contents))) 
      else Lwt.return () 
    in
    match (try Some (List.find (fun x -> Blkfront.id x = devid) context) with _ -> None) with
    | Some blkif ->
      let page = Io_page.get 1 in
      Io_page.string_blit contents 0 page 0 4096;
      lwt _ = Blkfront.write blkif sector [Io_page.to_cstruct page] in
      Lwt.return ()
    | None -> Lwt.return ()

  let read_sector context devid sector =
    match (try Some (List.find (fun x -> Blkfront.id x = devid) context) with _ -> None) with
    | Some blkif ->
      lwt info = Blkfront.get_info blkif in
      let page = Io_page.(to_cstruct (get 1)) in
      let mysector = Cstruct.sub page 0 info.Blkfront.sector_size in
      let stream = Blkfront.read blkif sector [mysector] in
      let strings = Cstruct.to_string mysector in
      Lwt.return (Cohttp.Base64.encode strings)
    | None ->
      Lwt.fail (Failure "No blkif")

  let list context () =
    let devids = List.map (fun x -> Blkfront.id x) context in
    Lwt.return devids

  let start_hammer context devid = 
    (*    let _ = Block.block_hammer () in*)
    Lwt.return ()

  let stop_hammer context devid = 
    Lwt.return ()

  let start_tickle context devid =
    (*    let _ = Block.block_tickle () in*)
    Lwt.return ()

  let stop_tickle context devid =
    Lwt.return ()

  let start_junk_writer context vbdid seed =
    Lwt.return ()

  let stop_junk_writer context vbdid =
    Lwt.return true

end

module Vif = struct
  let list context () =
    (*    match !net_manager with 
          | Some t ->
          let vifs = Net.Manager.get_intfs t in
          Lwt.return (List.map (fun (x,y) -> OS.Netif.string_of_id x) vifs)
          | None -> *)
    Lwt.return []

  let get_ipv4 context vifid =
    (*    match !net_manager with 
          | Some t ->
          let id = OS.Netif.id_of_string vifid in
          let addr = Net.Manager.get_intf_ipv4addr t id in
          Lwt.return (Ipaddr.V4.to_string addr)
          | None ->*)
    Lwt.return ""

  let inject_packet context vifid packet = 
    Lwt.return ()

end
