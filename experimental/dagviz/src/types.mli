open! Core

module type Name = sig
  module Count : sig
    type t

    val zero : t
    val succ : t -> t
  end

  type t [@@deriving sexp_of]

  val to_string : t -> string
  val create : unit -> t
  val next : Count.t -> t * Count.t

  include Comparable.S_plain with type t := t
end

module Default_id : Name

module Make (Name : Name) : sig
  module rec Kind : sig
    type t =
      | Bindings of
          { bindings : Binding.t list
          ; last_body : Computation.t
          }
      | Value of Value.t
      | Wrapping of
          { name : string
          ; introduces : Name.t list
          ; bodies : Computation.t list
          }
  end

  and Binding : sig
    type t =
      { bound : Computation.t
      ; as_ : Name.t
      }
    [@@deriving sexp_of, compare]
  end

  and Value : sig
    type value_without_position =
      | Fake
      | Redirect of { name : Name.t }
      | Named of Name.t
      | Singleton
      | Mapn of Value.t list
    [@@deriving sexp_of, compare]

    type t =
      { value_kind : value_without_position
      ; value_here : Source_code_position.Stable.V1.t option
      ; value_id : Name.t
      }
    [@@deriving sexp_of, compare]
  end

  and Computation : sig
    type nonrec t =
      { kind : Kind.t
      ; free_variables : Name.Set.t
      ; here : Source_code_position.Stable.V1.t option
      }
    [@@deriving sexp_of, compare]
  end

  class ['acc] fold : object
    method binding : Binding.t -> 'acc -> 'acc
    method computation : Computation.t -> 'acc -> 'acc
    method kind : Kind.t -> 'acc -> 'acc
    method name : Name.t -> 'acc -> 'acc
    method position : Lexing.position -> 'acc -> 'acc
    method string : string -> 'acc -> 'acc
    method value : Value.t -> 'acc -> 'acc
    method value_kind : Value.value_without_position -> 'acc -> 'acc
  end
end
