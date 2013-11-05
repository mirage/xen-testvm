open Cmdliner
open Lwt

(*
module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
module V = Vchan.Make(Xs)
module Vchan_http = Vchan_http.Make(V)
module RpcM = Vchan_http.RpcM
module Client = Test_interface.ClientM(RpcM)
*)

let pool = "http://st30.uk.xensource.com/"
let username = "root"
let password = "xenroot"

let start kernel =
  Printf.fprintf stderr "start kernel=%s\n%!" kernel;
  let t =
    Boot_disk.upload ~pool ~username ~password ~kernel >>= fun vdi ->
    let open Xen_api in
    let open Xen_api_lwt_unix in
    let rpc = make pool in
    Session.login_with_password rpc username password "1.0" >>= fun session_id ->
    let none = API.Ref.of_string "none" in
    let m = Int64.(mul (mul 256L 1024L) 1024L) in
    try_lwt
      (* Someone, please, add some default arguments!! *)
      VM.create ~rpc ~session_id
       ~name_label:"storage_test"
       ~name_description:"This VM is part of the storage_test, based on xen-testvm"
       ~user_version:1L ~is_a_template:false ~affinity:none
       ~memory_target:m ~memory_static_max:m ~memory_dynamic_max:m
       ~memory_dynamic_min:m ~memory_static_min:m ~vCPUs_params:[]
       ~vCPUs_max:1L ~vCPUs_at_startup:1L ~actions_after_shutdown:`destroy
       ~actions_after_reboot:`restart ~actions_after_crash:`destroy
       ~pV_bootloader:"pygrub" ~pV_kernel:"" ~pV_ramdisk:"" ~pV_args:""
       ~pV_bootloader_args:"" ~pV_legacy_args:"" ~hVM_boot_policy:""
       ~hVM_boot_params:[] ~hVM_shadow_multiplier:1. ~platform:[] ~pCI_bus:""
       ~other_config:[] ~recommendations:"" ~xenstore_data:[] ~ha_always_run:false
       ~ha_restart_priority:"" ~tags:[] ~blocked_operations:[]
       ~protection_policy:none ~is_snapshot_from_vmpp:false
       ~appliance:none ~start_delay:0L ~shutdown_delay:0L
       ~order:0L ~suspend_SR:none ~version:1L >>= fun vm ->
      VBD.create ~rpc ~session_id
        ~vM:vm ~vDI:vdi ~userdevice:"0" ~bootable:true ~mode:`RW ~_type:`Disk
        ~unpluggable:false ~empty:false ~other_config:["owner", "true"]
        ~qos_algorithm_type:"" ~qos_algorithm_params:[] >>= fun vbd ->
      VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false >>= fun () ->
      VM.get_domid ~rpc ~session_id ~self:vm >>= fun domid ->
      VM.get_uuid ~rpc ~session_id ~self:vm >>= fun uuid ->
      Printf.fprintf stderr "VM.uuid=%s\nVM.domid=%Ld\n%!" uuid domid;
      return ()
    finally
      Session.logout ~rpc ~session_id in
  Lwt_main.run t;
  `Ok ()

let kernel_filename =
  let doc = "Specify the kernel filename" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILENAME")

let start_cmd =
  let man =
    [ `S "DESCRIPTION";
      `P "Upload a kernel and boot it.";
    ] in
  let doc = "Upload a kernel and boot it" in
  Term.(ret (pure start $ kernel_filename)),
  Term.info "start" ~doc ~man

let default = 
  let doc = "Perform a set of storage datapath tests" in 
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "storage_test" ~version:"0.0.1" ~doc
    
let commands = [start_cmd] 

let _ =
  match Term.eval_choice ~catch:true default commands with
  | `Ok x -> x
  | _ -> exit 1
