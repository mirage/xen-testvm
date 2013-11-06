open Cmdliner
open Lwt

module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
module V = Vchan.Make(Xs)
module Vchan_http = Vchan_http.Make(V)
module RpcM = Vchan_http.RpcM
module Client = Test_interface.ClientM(RpcM)

open Xen_api
open Xen_api_lwt_unix

let pool = "http://localhost/"
let username = "root"
let password = ref ""

(**************************************************************************)
(* Testcase definitions                                                   *)
open OUnit

let start_shutdown ~rpc ~session_id vm = return ()

let reboot ~rpc ~session_id vm =
  VM.clean_reboot ~rpc ~session_id ~vm

let suspend_resume ~rpc ~session_id vm =
  VM.suspend ~rpc ~session_id ~vm >>= fun () ->
  VM.resume ~rpc ~session_id ~vm ~start_paused:false ~force:false

let use vm test_case () =
  let t =
    let rpc = make pool in
    Session.login_with_password rpc username !password "1.0" >>= fun session_id ->
    try_lwt
      Test_vm.start ~rpc ~session_id vm >>= fun () ->
      test_case ~rpc ~session_id vm
    finally
      Test_vm.shutdown ~rpc ~session_id vm >>= fun () ->
      Session.logout ~rpc ~session_id in
  Lwt_main.run t
 
let suite vm = "lifecycle" >::: [
  "start_shutdown" >:: (use vm start_shutdown);
  "reboot"         >:: (use vm reboot);
  "suspend_resume" >:: (use vm suspend_resume);
]

(**************************************************************************)
(* Setup and teardown                                                     *)

let start kernel passw verbose =
  password := passw;
  let vm = Lwt_main.run (Test_vm.create kernel !password) in
  run_test_tt ~verbose (suite vm);
  let _ = Lwt_main.run (Test_vm.destroy vm !password) in
  `Ok ()

let kernel_filename =
  let doc = "Specify the kernel filename" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILENAME")

let password =
  let doc = "Local root password" in
  Arg.(required & opt (some string) (Some "root") & info ["password"; "p"] ~doc ~docv:"PASSWORD")

let verbose =
  let doc = "Verbose output from the tests" in
  Arg.(required & opt (some bool) (Some false) & info ["verbose"; "v"] ~doc ~docv:"VERBOSE")

let start_cmd =
  let man =
    [ `S "DESCRIPTION";
      `P "Run a series of VM lifecycle tests using a simple Mirage kernel.";
    ] in
  let doc = "Upload a kernel and boot it" in
  Term.(ret (pure start $ kernel_filename $ password $ verbose)),
  Term.info "start" ~doc ~man

let _ =
  match Term.eval ~catch:true start_cmd with
  | `Ok x -> x
  | _ -> exit 1
