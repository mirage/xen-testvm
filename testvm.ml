open Lwt (* provides >>= and join *)
open OS  (* provides Time, Console and Main *)
open Printf

let rec ticker () =
	Time.sleep 1.0 >>
	  Xs.(wait (fun h ->
	  lwt state = try_lwt read h "hello" with _ -> Lwt.return "" in
	  return (state="there"))) >>
	    Console.log_s "Tick" >> 
    ticker ()

let main () =
  Random.self_init ();
  let _ = Mirage_guest_agent.control_watch () in
  let _ = Block.block_tickle () in
(*  lwt junk = Block.junk_writer () in*)
  ticker ()
(*  let _ = Mirage_guest_agent.test_guest_agent () in*)


