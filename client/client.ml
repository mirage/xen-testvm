
module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
module V = Vchan.Make(Xs)
module Vchan_http = Vchan_http.Make(V)
module RpcM = Vchan_http.RpcM
module Client = Test_interface.ClientM(RpcM)
      
let main () =
  let evtchn_h = Eventchn.init () in
  let domid = int_of_string Sys.argv.(1) in
  lwt vch = V.client ~evtchn_h ~domid ~xs_path:(Printf.sprintf "/local/domain/%d/data/vchan" domid) in
  RpcM.vch := Some vch;
  lwt () = Client.Block.tickle () in
  Lwt.return ()

let _ =
  Lwt_main.run (main ())
