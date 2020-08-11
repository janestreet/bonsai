open! Core_kernel
open! Import

module Result_spec : sig
  module type S = sig
    type t
    type incoming

    val view : t -> string
    val incoming : t -> incoming -> Event.t
  end

  type ('result, 'incoming) t =
    (module S with type t = 'result and type incoming = 'incoming)

  (** [include No_incoming] is a quick way to define a [Result_spec] with no incoming
      events:

      {[
        module Plain_int_result : Result_spec = struct
          type t = int

          let view = Int.to_string

          include Bonsai_test.Proc.Result_spec.No_incoming
        end
      ]}
  *)
  module No_incoming : sig
    type incoming = Nothing.t

    val incoming : _ -> Nothing.t -> Event.t
  end

  module type Sexpable = sig
    type t [@@deriving sexp_of]
  end

  module type Stringable = sig
    type t

    val to_string : t -> string
  end

  val sexp : (module Sexpable with type t = 'a) -> ('a, Nothing.t) t
  val string : (module Stringable with type t = 'a) -> ('a, Nothing.t) t
end

module Handle : sig
  type ('result, 'incoming) t

  val show : _ t -> unit
  val result : ('result, _) t -> 'result
  val do_actions : (_, 'incoming) t -> 'incoming list -> unit

  val create
    :  ('result, 'incoming) Result_spec.t
    -> 'result Bonsai.Computation.t
    -> ('result, 'incoming) t

  val show_model : _ t -> unit
  [@@alert
    rampantly_nondeterministic
      "This function exposes Bonsai internals that may change without warning"]
end
