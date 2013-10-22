open OS

type context = unit
type 'a t = 'a Lwt.t
let bind = Lwt.bind
let return = Lwt.return
let fail = Lwt.fail
let handle_failure = Lwt.catch

let (>>=) = bind

let net_manager = ref None

let shutdown context () = 
  Console.log_s "shutdown" >>= fun _ ->
  Sched.shutdown Sched.Poweroff;
  Lwt.return ()
    
let reboot context () =
  Console.log_s "reboot" >>= fun _ ->
  Sched.shutdown Sched.Reboot;
  Lwt.return ()
    
let crash context () =
  Console.log_s "crash" >>= fun _ ->
  Sched.shutdown Sched.Reboot;
  Lwt.return ()

module Vbd = struct
  let write_sector context devid sector contents = 
    let contents = Cohttp.Base64.decode contents in
    lwt () =
      if String.length contents <> 512
      then Lwt.fail (Failure (Printf.sprintf "Expecting 512 bytes of contents (got %d)" (String.length contents))) 
      else Lwt.return () in
    lwt Some blkif = OS.Devices.find_blkif devid in
    (* We need to do a read/modify/write *)
    let round_to_page = Int64.(shift_left (shift_right sector 2) 2) in
    let offset_within_page = Int64.(to_int (sub sector round_to_page)) in
    let stream = blkif#read_512 round_to_page 8L in
    lwt blocks = Lwt_stream.to_list stream in
    (* since our request was page aligned, we've got a single page *)
    assert (List.length blocks = 1);
    let block = List.hd blocks in
    assert (block.Cstruct.off = 0);
    Cstruct.blit_from_string contents 0 block (offset_within_page * 512) 512;
    lwt () = blkif#write_page (Int64.mul round_to_page 512L) block.Cstruct.buffer in
    Lwt.return ()

  let read_sector context devid sector =
    lwt Some blkif = OS.Devices.find_blkif devid in
    let stream = blkif#read_512 sector 1L in
    lwt list = Lwt_stream.to_list stream in
    let strings = List.map Cstruct.to_string list in
    Lwt.return (Cohttp.Base64.encode (String.concat "" strings))

  let list context () =
    let devids = !(Block.block_devices) in
    Lwt.return devids

  let start_hammer context devid = 
    let _ = Block.block_hammer () in
    Lwt.return ()

  let stop_hammer context devid = 
    Lwt.return ()

  let start_tickle context devid =
    Console.log_s "In block tickle..." >>= fun _ ->
    let _ = Block.block_tickle () in
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
    match !net_manager with 
    | Some t ->
      let vifs = Net.Manager.get_intfs t in
      Lwt.return (List.map (fun (x,y) -> OS.Netif.string_of_id x) vifs)
    | None -> 
      Lwt.return []

  let get_ipv4 context vifid =
    match !net_manager with 
    | Some t ->
      let id = OS.Netif.id_of_string vifid in
      let addr = Net.Manager.get_intf_ipv4addr t id in
      Lwt.return (Ipaddr.V4.to_string addr)
    | None ->
      Lwt.return ""

  let inject_packet context vifid packet = 
    Lwt.return ()

end
