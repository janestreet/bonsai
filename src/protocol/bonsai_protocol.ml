open! Core
open Bonsai.Private


module Entry = struct
  type t =
    { label : [ `Bonsai of Node_path.t | `Other of string ]
    ; entry_type : string
    ; start_time : float
    ; duration : float
    }
  [@@deriving bin_io, sexp]

  let%expect_test _ =
    print_endline [%bin_digest: t];
    [%expect {| 06de0862e532730a58840545d773281d |}]
  ;;
end

module Message = struct
  type t =
    | Graph_info of Graph_info.t
    | Performance_measure of Entry.t
  [@@deriving bin_io, sexp]

  let%expect_test _ =
    print_endline [%bin_digest: t];
    [%expect {| b17892a0948ebd34a0b716278484df52 |}]
  ;;
end
