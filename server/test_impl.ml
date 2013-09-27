open OS

type context = unit
type 'a t = 'a Lwt.t
let bind = Lwt.bind
let return = Lwt.return
let fail = Lwt.fail
let handle_failure = Lwt.catch

let (>>=) = bind

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

module Block = struct
  let hammer context () = 
    let _ = Block.block_hammer () in
    Lwt.return ()

  let tickle context () =
    let _ = Block.block_tickle () in
    Lwt.return ()

  let write_junk context size junk =
    let finished_t, u = Lwt.task () in
    let listen_t = OS.Devices.listen (fun id ->
	OS.Devices.find_blkif id >>= function
        | None -> return ()
        | Some blkif -> Lwt.wakeup u blkif; return ()
      ) in
    (* Get one device *)
    lwt blkif = finished_t in
    (* Cancel the listening thread *)
    Lwt.cancel listen_t;
    OS.Console.log_s (Printf.sprintf "size=%Ld" blkif#size) >>
    Junk.write_junk blkif (Int64.div blkif#size 4096L) size junk

end

