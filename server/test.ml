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

module V = struct
  include Vchan.Make(Activations)(Xs)
  type 'a io = 'a Lwt.t
  type buffer = Cstruct.t
  type flow = t
end
module Vchan_http = Vchan_http.Make(V)

let (>>=) = Lwt.bind

module RpcServer = Test_interface.ServerM(Test_impl)

let connect_blkifs xs =
  lwt blkifs = Xs.(immediate xs (fun h -> directory h "device/vbd")) in
  Lwt_list.fold_left_s (fun acc s -> lwt x = Blkfront.connect s in match x with | `Ok b -> Lwt.return (b::acc) | _ -> Lwt.return acc) [] blkifs

(*let net_thread () =
  Net.Manager.create (fun t interface id ->
      Test_impl.net_manager := Some t;
      Console.log_s "Got here...\n" >>
      Net.Manager.configure interface `DHCP >>
      let ipv4 = Net.Manager.get_ipv4 interface in
      let ipv4addr = Net.Ipv4.get_ip ipv4 in
      lwt () = Mirage_guest_agent.nic_update id (Ipaddr.V4.to_string ipv4addr) in
      Console.log_s (Printf.sprintf "IP: %s" (Ipaddr.V4.to_string ipv4addr))
    )*)

let rec serve_forever ctx vch =
  lwt () = Vchan_http.http_handler Jsonrpc.call_of_string Jsonrpc.string_of_response RpcServer.process vch ctx in
  serve_forever ctx vch
 
let with_vchan evtchn_h domid nodepath f =
  Printf.printf "Initializing Server domid=%d xs_path=%s\n%!" domid nodepath;
  V.server ~evtchn_h ~domid ~xs_path:nodepath
    ~read_size:4000 ~write_size:4000 ~persist:true
  >>= fun vch ->
  let flow = Vchan_http.openflow vch in
  Printf.printf "Initialization done!\n%!";
  f flow
      

open V1_LWT

let (>>=) = Lwt.bind

module Main (C:CONSOLE) = struct
  let buf = String.create 1

  let rec echo vch =
    Vchan_http.IO.read_into vch buf 0 (String.length buf) >>= fun () ->
    Vchan_http.IO.write_from vch buf 0 (String.length buf) >>= fun () ->
    echo vch

  let start c =
(*    Mirage_guest_agent.control_watch ();
    Mirage_guest_agent.dummy_guest_agent ();*)
    lwt xs = Xs.make () in
    (*lwt blkifs = connect_blkifs xs in*)
    ignore(with_vchan (Eventchn.init ()) 0 "data/vchan" (serve_forever [](*blkifs*)));
    lwt () = Time.sleep 1000.0 in
    Lwt.return ()

end


