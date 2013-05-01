open Lwt (* provides >>= and join *)
open OS  (* provides Time, Console and Main *)
open Printf

let udid = ref ""

let test_guest_agent () =
	let rec inner m =
		Console.log_s "Tick" >> 
			lwt () = Xs.(immediate (fun h -> write h "data/meminfo_free" (Printf.sprintf "%d" (1024*(m mod 1024))))) in
	        lwt () = try_lwt Xs.(immediate (fun h -> write h "data/meminfo_total" (Printf.sprintf "%d" (1024*((1023-m) mod 1024))))) with _ -> return () in
            lwt _ = try_lwt Xs.(immediate (fun h -> read h "foo")) with _ -> return "" in
            lwt _ = try_lwt Xs.(immediate (fun h -> read h "bar")) with _ -> return "" in
	        lwt myudid = Xs.(immediate (fun h -> read h "unique-domain-id")) in
            lwt () = 
					let kvs = [ "attr/PVAddons/MajorVersion","6";
								"attr/PVAddons/MinorVersion","1";
								"attr/PVAddons/MicroVersion","50";
								"data/os_name","Mirage";
								"data/os_majorver",(Printf.sprintf "%d" m) ] in
					Lwt_list.iter_s (fun (k,v) -> Xs.(immediate (fun h -> write h k v))) kvs in
            lwt () = Xs.(immediate (fun h -> write h "data/updated" (Printf.sprintf "%d" m))) in
            lwt () = Time.sleep 60.0 in
            inner (m+1)
    in inner 0

let suspend () =
  lwt cancelled = Sched.suspend () in
  Console.log (Printf.sprintf "cancelled=%d" cancelled);
  Lwt.return cancelled

let control_watch () = 
  lwt () = Console.log_s (Printf.sprintf "xs_watch ()") in
  let rec inner () = 
    lwt dir = Xs.(immediate (fun h -> directory h "control")) in
    lwt result =
      if List.mem "shutdown" dir then  begin
      lwt msg = try_lwt Xs.(immediate (fun h -> read h "control/shutdown")) with _ -> return "" in
      lwt () = Console.log_s (Printf.sprintf "Got control message: %s" msg) in
      match msg with
	  | "suspend" -> 
		  lwt () = Xs.(immediate (fun h -> rm h "control/shutdown")) in
          lwt _ = suspend () in
          lwt () = Console.log_s "About to read domid" in
          lwt domid = Xs.(immediate (fun h -> read h "domid")) in
          lwt () = Console.log_s (Printf.sprintf "We're back: domid=%s" domid) in
          return true
      | "poweroff" -> 
		  Sched.shutdown Sched.Poweroff;
		  return false (* Doesn't get here! *)
	  | "reboot" ->
		  Sched.shutdown Sched.Reboot;
		  return false (* Doesn't get here! *)
	  | "halt" ->
		  Sched.shutdown Sched.Poweroff;
		  return false
	  | "crash" ->
		  Sched.shutdown Sched.Crash;
		  return false
	  | _ -> 
		  return false
      end else return false
    in
    lwt () = Time.sleep 1.0 in
    inner ()
  in inner ()


