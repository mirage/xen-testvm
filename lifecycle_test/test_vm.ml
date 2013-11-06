(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Cmdliner
open Lwt

let pool = "http://localhost/"
let username = "root"

open Xen_api
open Xen_api_lwt_unix

let create kernel password =
  Boot_disk.upload ~pool ~username ~password ~kernel >>= fun vdi ->
  (* XXX: there is a race between the read/write VDI.attach caused by
     the upload and the read/only VDI.attach caused by the VM.start *)
  Unix.sleep 1; (* This is terrible *)
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
    return vm
  finally
    Session.logout ~rpc ~session_id

let shutdown ~rpc ~session_id vm =
    VM.get_power_state ~rpc ~session_id ~self:vm >>= fun power_state ->
    ( if power_state <> `Halted
      then VM.hard_shutdown ~rpc ~session_id ~vm
      else return () )

let destroy vm password =
  let rpc = make pool in
  Session.login_with_password rpc username password "1.0" >>= fun session_id ->
  try_lwt
    shutdown ~rpc ~session_id vm >>= fun () ->
    VM.get_VBDs ~rpc ~session_id ~self:vm >>= fun vbds ->
    Lwt_list.iter_s (fun vbd ->
      VBD.get_other_config ~rpc ~session_id ~self:vbd >>= fun other_config ->
      VBD.get_empty ~rpc ~session_id ~self:vbd >>= fun empty ->
      if not(empty) && (List.mem_assoc "owner" other_config) && (List.assoc "owner" other_config = "true") then begin
        VBD.get_VDI ~rpc ~session_id ~self:vbd >>= fun vdi ->
        VDI.destroy ~rpc ~session_id ~self:vdi
      end else return ()
    ) vbds >>= fun () ->
    VM.destroy ~rpc ~session_id ~self:vm
  finally
    Session.logout ~rpc ~session_id

let start ~rpc ~session_id vm =
  shutdown ~rpc ~session_id vm >>= fun () ->
  VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false
