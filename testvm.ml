open Lwt (* provides >>= and join *)
open OS  (* provides Time, Console and Main *)
open Printf

let read_line () =
  Time.sleep (Random.float 2.5) >>
  return (String.make (Random.int 20) 'a')

let rec echo_server =
  function
  |0 -> return ()
  |num_lines ->
    lwt s = read_line () in
    Console.log s;
    echo_server (num_lines - 1)

let suspend () =
  lwt cancelled = Sched.suspend () in
  Console.log (Printf.sprintf "cancelled=%d" cancelled);
  Lwt.return cancelled

let xs_watch () = 
  lwt () = Console.log_s (Printf.sprintf "xs_watch ()") in
  let rec inner () = 
    lwt dir = Xs.t.Xs.directory "control" in
    lwt result =
      if List.mem "shutdown" dir then  begin
      lwt msg = try_lwt Xs.t.Xs.read "control/shutdown" with _ -> return "" in
      lwt () = Console.log_s (Printf.sprintf "Got control message: %s" msg) in
      match msg with
      | "suspend" -> 
        lwt () = Xs.t.Xs.rm "control/shutdown" in
        lwt _ = suspend () in
        lwt () = Console.log_s "About to read domid" in
        Xs.check Xs.t;
        lwt domid = Xs.t.Xs.read "domid" in
        lwt () = Console.log_s (Printf.sprintf "We're back: domid=%s" domid) in
        return true
      | _ -> return false
      end else return false
    in
    lwt () = Time.sleep 1.0 in
    inner ()
  in inner ()

let block () =
 let finished_t, u = Lwt.task () in
  let listen_t = OS.Devices.listen (fun id ->
    OS.Devices.find_blkif id >>=
    function
    | None -> return ()
    | Some blkif -> Lwt.wakeup u blkif; return ()
  ) in
  (* Get one device *)
  lwt blkif = finished_t in
  (* Cancel the listening thread *)
  Lwt.cancel listen_t;
  printf "ID: %s\n%!" blkif#id;
  printf "Connected block device\n%!";
  printf "Total device size = %Ld\nSector size = %d\n%!" blkif#size blkif#sector_size;
  printf "Device is read%s\n%!" (if blkif#readwrite then "/write" else "-only");

  let n = 31 in
  let liveness = Array.init n (fun _ -> 0) in

  let rec live_print () =
    let l = Array.to_list liveness in
    Array.iteri (fun i _ -> liveness.(i) <- 0) liveness;
    let s = String.concat " " (List.map string_of_int l) in
    lwt () = Console.log_s (Printf.sprintf "%f %s" (Clock.time ()) s) in
    Time.sleep 1.0 >> live_print ()
  in

  let _ = live_print () in

  let dump_page p =
    let s = ref [] in
    let b = Buffer.create 128 in
    for i=0 to 1023 do
      if i>0 && i mod 32 = 0 then begin
	s := (Buffer.contents b) :: !s;
	Buffer.clear b;
      end;
      Buffer.add_string b (Printf.sprintf "%02x " (Char.code p.[i]))
    done;
    let rec inner s =
      match s with 
	| s::ss ->
	  Console.log_s s >> Time.sleep 0.1 >> inner ss
	| [] -> 
	  Lwt.return ()
    in inner (List.rev !s)
  in

  let rec inner page n i =
    let start = Int64.mul 8L (Int64.of_int n) in
    page.{i mod 4096} <- Char.chr (i mod 256);
    lwt () = blkif#write_page (Int64.mul start 512L) page in
    lwt p = Lwt_stream.to_list (blkif#read_512 start 8L) in
    let newpage = Io_page.to_string (List.hd p) in
    let oldpage = Io_page.to_string page in
(*    lwt () = 
	if newpage <> oldpage 
	then 
	  Console.log_s "Error! new:" >> 
	    dump_page newpage >> 
	    Time.sleep 1.0 >> 
	    Console.log_s "old:" >> 
	    dump_page oldpage  >>
	    Time.sleep 10.0 
	else return () in*)

    liveness.(n) <- liveness.(n) + 1;
    inner page n (i+1);
  in 

  for i=0 to n-1 do
    ignore(inner (Io_page.get ()) i 0);
  done;

  Lwt.return ()



let main () =
  Random.self_init ();
  let _ = xs_watch () in
  block ();
  echo_server 1000

let _ = OS.Main.run (main ())
