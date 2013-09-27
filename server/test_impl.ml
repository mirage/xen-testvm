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
  let hammer () = 
    Block.block_hammer ()

end

