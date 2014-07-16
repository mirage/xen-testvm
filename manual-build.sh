ocamlfind ocamldep -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -modules main.ml > main.ml.depends
ocamlfind ocamldep -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -modules test.ml > test.ml.depends
ocamlfind ocamldep -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -modules test_impl.ml > test_impl.ml.depends
ocamlfind ocamlc -c -g -annot -bin-annot -principal -strict-sequence -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -o test_impl.cmo test_impl.ml
ocamlfind ocamlc -c -g -annot -bin-annot -principal -strict-sequence -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -o test.cmo test.ml
ocamlfind ocamlc -c -g -annot -bin-annot -principal -strict-sequence -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -o main.cmo main.ml
ocamlfind ocamlopt -c -g -annot -bin-annot -principal -strict-sequence -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -o test_impl.cmx test_impl.ml
ocamlfind ocamlopt -c -g -annot -bin-annot -principal -strict-sequence -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -o test.cmx test.ml
ocamlfind ocamlopt -c -g -annot -bin-annot -principal -strict-sequence -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o -o main.cmx main.ml
ocamlfind ocamlopt -g -linkpkg -dontlink unix -output-obj -package vchan -package testvmlib -package mirage-types.lwt -package mirage-console-xen -package mirage-block-xen.front -package lwt.syntax -syntax camlp4o test_impl.cmx test.cmx main.cmx -o main.native.o

XEN_LIBDIR=$(ocamlfind query mirage-xen)
ld -d -nostdlib -m elf_x86_64 -T ${XEN_LIBDIR}/mirage-x86_64.lds ${XEN_LIBDIR}/x86_64.o \
          main.native.o ${XEN_LIBDIR}/libocaml.a ${XEN_LIBDIR}/libxen.a \
          ${XEN_LIBDIR}/libxencaml.a ${XEN_LIBDIR}/libdiet.a ${XEN_LIBDIR}/libm.a \
          ${XEN_LIBDIR}/longjmp.o -o mir-test.xen
