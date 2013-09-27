
open OS

module V = Vchan.Make(Xs)
module Vchan_http = Vchan_http.Make(V)

let (>>=) = Lwt.bind

module RpcServer = Test_interface.ServerM(Test_impl)


let net_thread () =
  Net.Manager.create (fun t interface id ->
      Console.log_s "Got here...\n" >>
      Net.Manager.configure interface `DHCP >>
      let ipv4 = Net.Manager.get_ipv4 interface in
      let ipv4addr = Net.Ipv4.get_ip ipv4 in
      lwt () = Mirage_guest_agent.nic_update id (Ipaddr.V4.to_string ipv4addr) in
      Console.log_s (Printf.sprintf "IP: %s" (Ipaddr.V4.to_string ipv4addr))
    )

let rec serve_forever vch =
  lwt () = Vchan_http.http_handler Jsonrpc.call_of_string Jsonrpc.string_of_response RpcServer.process vch in
  serve_forever vch
 
let with_vchan evtchn_h domid nodepath f =
  Printf.printf "Initializing Server domid=%d xs_path=%s\n%!" domid nodepath;
  V.server ~evtchn_h ~domid ~xs_path:nodepath
    ~read_size:5000 ~write_size:5000 ~persist:true
  >>= fun vch ->
  Printf.printf "Initialization done!\n%!";
  f vch
      
let main () =
  net_thread ();
  Mirage_guest_agent.control_watch ();
  Mirage_guest_agent.dummy_guest_agent ();
  with_vchan (Eventchn.init ()) 0 "data/vchan" serve_forever 
