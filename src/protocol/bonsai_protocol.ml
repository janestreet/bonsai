module Stable = struct
  open! Core.Core_stable
  open Bonsai.Stable.Private

  module Entry = struct
    module V1 = struct
      type t =
        { label : [ `Bonsai of Node_path.V1.t | `Other of string ]
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
  end

  module Message = struct
    module V1 = struct
      type t =
        | Graph_info of Graph_info.V1.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp, stable_variant]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b17892a0948ebd34a0b716278484df52 |}]
      ;;
    end

    module V2 = struct
      type t =
        | Graph_info of Graph_info.V2.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp, stable_variant ~version:V1.t ~modify:[ Graph_info ]]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| acf03a1188bfb7efeb8af957c2f31a09 |}]
      ;;

      let to_v1 t =
        to_V1_t t ~modify_Graph_info:(fun graph_info ->
          Graph_info (Graph_info.V2.to_v1 graph_info))
      ;;

      let of_v1 t =
        of_V1_t t ~modify_Graph_info:(fun graph_info ->
          Graph_info (Graph_info.V2.of_v1 graph_info))
      ;;
    end
  end
end

open! Core
open! Stable
open Bonsai.Private

module Versioned_message = struct
  type t =
    | V1 of Message.V1.t list
    | V2 of Message.V2.t list
  [@@deriving sexp, bin_io]
end

module Entry = struct
  type t = Entry.V1.t =
    { label : [ `Bonsai of Node_path.t | `Other of string ]
    ; entry_type : string
    ; start_time : float
    ; duration : float
    }
  [@@deriving bin_io, sexp]
end

module Message = struct
  type t = Message.V2.t =
    | Graph_info of Graph_info.t
    | Performance_measure of Entry.t
  [@@deriving bin_io, sexp]
end
