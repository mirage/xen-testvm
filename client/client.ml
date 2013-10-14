open Cmdliner

module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
module V = Vchan.Make(Xs)
module Vchan_http = Vchan_http.Make(V)
module RpcM = Vchan_http.RpcM
module Client = Test_interface.ClientM(RpcM)


let vif_list () =
  lwt vifs = Client.Vif.list () in
  List.iter (fun x -> Printf.printf "%s\n%!" x) vifs;
  Lwt.return ()

let vif_get_ipv4 vifid =
  Printf.printf "Getting ip...\n%!";
  lwt ipv4 = Lwt.catch (fun () -> Client.Vif.get_ipv4 vifid) (fun exn ->
  Printf.printf "Got exception: %s\n" (Printexc.to_string exn);
  Lwt.return "") in
  Printf.printf "Got result: '%s'\n%!" ipv4;
  lwt () = Lwt_unix.sleep 1.0 in
  Lwt.return ()

let vif_inject_packet vifid filename =
  lwt fd = Lwt_unix.openfile filename [Unix.O_RDONLY] 0o644 in
  let channel = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  lwt str = Lwt_io.read channel in
  Client.Vif.inject_packet vifid str

let block_write_sector vbdid offset filename =
  lwt fd = Lwt_unix.openfile filename [Unix.O_RDONLY] 0o644 in
  let channel = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  lwt str = Lwt_io.read channel in
  Printf.fprintf stderr "Read %d bytes\n%!" (String.length str);
  let str = Cohttp.Base64.encode str in
  Client.Vbd.write_sector vbdid offset str

let block_read_sector vbdid offset filename =
  lwt sector = Client.Vbd.read_sector vbdid offset in
  lwt fd = Lwt_unix.openfile filename [Unix.O_RDWR] 0o644 in
  let channel = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  let sector = Cohttp.Base64.decode sector in
  lwt () = Lwt_io.write channel sector in
  Lwt_io.close channel

let block_list () =
  lwt list = Client.Vbd.list () in
  List.iter (fun x -> Printf.printf "%s\n" x) list;
  Lwt.return ()

let block_start_junk_writer vbdid seed =
  Client.Vbd.start_junk_writer vbdid seed

let block_stop_junk_writer vbdid =
  lwt result = Client.Vbd.stop_junk_writer vbdid in
  Printf.printf "%b\n" result;
  Lwt.return ()


let copts_section="COMMON OPTIONS"

let domid_arg =
  let doc = "Specify the domain ID of the test VM" in
  Arg.(required & opt (some int) None & info [ "d"; "domid" ] ~docv:"DOMID"
         ~doc) 

let vifid_arg = 
  let doc = "Specify the vif" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VIFID")

let filename_arg n =
  let doc = "Specify the filename" in
  Arg.(required & pos n (some string) None & info [] ~doc ~docv:"FILENAME")

let vbdid_arg =
  let doc = "Specify the vbd" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VBDID")

let sector_arg =
  let doc = "Specify the sector offset" in
  Arg.(value & opt int64 0L & info ["s"; "sector"] ~doc ~docv:"SECTOR")


let vif_list_cmd =
  let man = 
    [`S "DESCRIPTION";
     `P "List the VIFs visible to the VM";
    ] in
  let doc = "List the VIFs visible to the VM" in
  Term.(ret(pure (fun domid -> `Ok (vif_list, domid)) $ domid_arg)), 
  Term.info "vif-list" ~doc ~man

let vif_get_ipv4_cmd =
  let man = 
    [`S "DESCRIPTION";
     `P "Get the IPV4 address of the specified device"; ] in
  let doc = "Get the IPV4 address of the specified device" in
  Term.(ret(pure (fun vifid domid -> `Ok ((fun () -> vif_get_ipv4 vifid), domid)) $ vifid_arg $ domid_arg)),
  Term.info "vif-get-ipv4" ~doc ~man

let vif_inject_packet_cmd =
  let man =
    [ `S "DESCRIPTION";
      `P "Causes the test VM to write the contents of the specified file out as a network packet on the specified VIF.";
    `P "The value VIF IDs can be obtained by the vif-list subcommand"] in
  let doc = "Output the file on the specified VIF" in
  Term.(ret(pure (fun vifid filename domid -> `Ok ((fun () -> vif_inject_packet vifid filename), domid)) $ vifid_arg $ filename_arg 1 $ domid_arg)),
  Term.info "vif-inject-packet" ~doc ~man

let block_write_sector_cmd =
  let man =
    [ `S "DESCRIPTION";
      `P "Executing this command will ask the VM to output the contents of 'FILENAME' into the specified VBD device at sector number SECTOR";
      `P "Note that the file specified must be exactly 256 bytes long"; ] in
  let doc = "Output the file on the specified VBD at the specified sector number" in
  Term.(ret(pure (fun vbdid sector filename domid -> `Ok ((fun () -> block_write_sector vbdid sector filename), domid)) $ vbdid_arg $ sector_arg $ filename_arg 1 $ domid_arg)),
  Term.info "block-write-sector" ~doc ~man

let block_read_sector_cmd =
  let man =
    [ `S "DESCRIPTION";
      `P "Executing this will ask the VM to read a sector from the specified VBD, and will write the output to FILENAME";
    ] in
  let doc = "Read 1 sector at the specifed offset from the VBD and write it to FILENAME" in
  Term.(ret (pure (fun vbdid sector filename domid -> `Ok ((fun () -> block_read_sector vbdid sector filename), domid)) $ vbdid_arg $sector_arg $ filename_arg 1 $domid_arg)),
  Term.info "block-read-sector" ~doc ~man

let block_list_cmd =
  let man =
    [ `S "DESCRIPTION";
      `P "List the block devices visible to the VM";
    ] in
  let doc = "List the block devices visible to the VM" in
  Term.(ret (pure (fun domid -> `Ok (block_list, domid)) $ domid_arg)),
  Term.info "block-list" ~doc ~man

let default = 
  let doc = "a test VM control system" in 
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "testcli" ~version:"0.0.1" ~doc
     

let _ =
  let (fn, domid) = (match Term.eval_choice ~catch:true default 
                             [vif_list_cmd;
                              vif_get_ipv4_cmd;
                              vif_inject_packet_cmd;
                              block_write_sector_cmd;
                              block_read_sector_cmd;
			      block_list_cmd;
                             ] with `Ok x -> x | _ -> exit 1) in

  let thread = 
    let evtchn_h = Eventchn.init () in
    lwt vch = V.client ~evtchn_h ~domid ~xs_path:(Printf.sprintf "/local/domain/%d/data/vchan" domid) in
    RpcM.vch := Some vch;
    fn ()
  in

  Lwt_main.run thread
