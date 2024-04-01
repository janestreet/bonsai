(* A collection of bonsai type signatures that we'd like to stay up-to-date in the guide
   code. *)

open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect
module Computation_status := Bonsai.Computation_status
module Url_var := Bonsai_web_ui_url_var

(* $MDX part-begin=assoc *)
val assoc
  :  ('k, 'cmp) Bonsai.comparator
  -> ('k, 'v, 'cmp) Map.t Bonsai.t
  -> f:('k Bonsai.t -> 'v Bonsai.t -> Bonsai.graph -> 'result Bonsai.t)
  -> Bonsai.graph
  -> ('k, 'result, 'cmp) Map.t Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=state_machine0 *)
val state_machine0
  :  default_model:'model
  -> apply_action:('action Bonsai.Apply_action_context.t -> 'model -> 'action -> 'model)
  -> Bonsai.graph
  -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=peek *)
val peek : 'a Bonsai.t -> Bonsai.graph -> 'a Computation_status.t Effect.t Bonsai.t
(* $MDX part-end *)

module Url_var : sig
  (* $MDX part-begin=url_var_components *)
  module Components : sig
    type t =
      { path : string
      ; query : string list String.Map.t
      ; fragment : string option
      }
  end

  (* $MDX part-end *)
  (* $MDX part-begin=url_var_from_handwritten *)
  module type T = sig
    type t [@@deriving sexp, equal]
  end

  module type S = sig
    include T

    val parse_exn : Components.t -> t
    val unparse : t -> Components.t
  end

  val create_exn : (module S with type t = 'a) -> fallback:'a -> 'a Url_var.t
  (* $MDX part-end *)

  (* $MDX part-begin=url_var_from_uri_parsing *)
  module Typed : sig
    val make
      :  ?on_fallback_raises:'a
      -> ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> (module T with type t = 'a)
      -> 'a Uri_parsing.Versioned_parser.t
      -> fallback:(Exn.t -> Components.t -> 'a)
      -> 'a Url_var.t
  end
  (* $MDX part-end *)

  (* $MDX part-begin=url_var_usage_api *)
  val value : 'a Url_var.t -> 'a Bonsai.t
  val set_effect : ?how:[ `Push | `Replace ] -> 'a Url_var.t -> 'a -> unit Effect.t
  (* $MDX part-end *)
end
