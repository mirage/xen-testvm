open Mirage

let main =
  foreign "Test.Main" (console @-> job)

let () =
  add_to_ocamlfind_libraries ["vchan"; "testvmlib"; "mirage-block-xen.front"];
  register "test" [
    main $ default_console
  ]
