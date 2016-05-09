open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

let ctypes_libdir = Sys.getenv "CTYPES_LIB_DIR" in

dispatch begin
  function
  | After_rules ->

    flag ["compile"; "c"; "use_ctypes"]
      (S[A"-I"; A ctypes_libdir]);

    flag ["ocaml"; "link"; "byte"; "library"; "use_posix_types_stubs"] &
      S[A"-dllib"; A"-lposix_types_stubs"];

    flag ["ocaml"; "link"; "native"; "library"; "use_posix_types_stubs"] &
      S[A"-cclib"; A"-lposix_types_stubs"];
  | _ -> ()
end;;
