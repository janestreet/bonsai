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

module Default_id = struct
  module Count = Int

  module T = struct
    type t =
      | User of int
      | Gen of int
    [@@deriving bin_io, sexp, compare]
  end

  include T
  include Comparable.Make_binable (T)

  let count = ref 0

  let create () =
    count := !count + 1;
    User !count
  ;;

  let next count =
    let count = Count.succ count in
    Gen count, count
  ;;

  let to_string = function
    | User x -> Int.to_string x
    | Gen x -> "gen_" ^ Int.to_string x
  ;;
end

module Make (Name : Name) = struct
  (* This ugly recursive type / recursive module structure is
     required in order to get sexp-deriving to work correctly *)

  type kind =
    | Bindings of
        { bindings : binding list
        ; last_body : computation
        }
    | Value of value
    | Wrapping of
        { name : string
        ; introduces : Name.t list
        ; bodies : computation list
        }

  and binding =
    { bound : computation
    ; as_ : Name.t
    }

  and value_without_position =
    | Fake
    | Redirect of { name : Name.t }
    | Named of Name.t
    | Singleton
    | Mapn of value list

  and value =
    { value_kind : value_without_position
    ; value_here : Source_code_position.Stable.V1.t option
    ; value_id : Name.t
    }

  and computation =
    { kind : kind
    ; free_variables : Name.Set.t
    ; here : Source_code_position.Stable.V1.t option
    }
  [@@deriving sexp_of, compare]

  module rec Kind : sig
    type t = kind =
      | Bindings of
          { bindings : binding list
          ; last_body : computation
          }
      | Value of value
      | Wrapping of
          { name : string
          ; introduces : Name.t list
          ; bodies : computation list
          }
    [@@deriving sexp]
  end =
    Kind

  and Binding : sig
    type t = binding =
      { bound : computation
      ; as_ : Name.t
      }
    [@@deriving sexp_of, compare]
  end = struct
    type t = binding =
      { bound : computation
      ; as_ : Name.t
      }
    [@@deriving sexp_of, compare]
  end

  and Value : sig
    type nonrec value_without_position = value_without_position =
      | Fake
      | Redirect of { name : Name.t }
      | Named of Name.t
      | Singleton
      | Mapn of value list
    [@@deriving sexp_of, compare]

    and t = value =
      { value_kind : value_without_position
      ; value_here : Source_code_position.t option
      ; value_id : Name.t
      }
    [@@deriving sexp_of, compare]
  end = struct
    type nonrec value_without_position = value_without_position =
      | Fake
      | Redirect of { name : Name.t }
      | Named of Name.t
      | Singleton
      | Mapn of value list
    [@@deriving sexp_of, compare]

    and t = value =
      { value_kind : value_without_position
      ; value_here : Source_code_position.Stable.V1.t option
      ; value_id : Name.t
      }
    [@@deriving sexp_of, compare]
  end

  and Computation : sig
    type nonrec t = computation =
      { kind : kind
      ; free_variables : Name.Set.t
      ; here : Source_code_position.t option
      }
    [@@deriving sexp_of, compare]
  end = struct
    type nonrec t = computation =
      { kind : kind
      ; free_variables : Name.Set.t
      ; here : Source_code_position.Stable.V1.t option
      }
    [@@deriving sexp_of, compare]
  end

  class ['acc] fold =
    object (self)
      method name : Name.t -> 'acc -> 'acc = fun _ acc -> acc
      method position : Source_code_position.t -> 'acc -> 'acc = fun _ acc -> acc

      method value_kind : Value.value_without_position -> 'acc -> 'acc =
        fun value_kind acc ->
          match value_kind with
          | Fake -> acc
          | Redirect { name } -> self#name name acc
          | Named name -> self#name name acc
          | Singleton -> acc
          | Mapn values ->
            List.fold ~init:acc ~f:(fun acc value -> self#value value acc) values

      method value : Value.t -> 'acc -> 'acc =
        fun value acc ->
          match value with
          | { value_kind; value_here; value_id } ->
            self#value_kind value_kind acc
            |> fun acc ->
            Option.value_map
              value_here
              ~f:(fun value_here -> self#position value_here acc)
              ~default:acc
            |> self#name value_id

      method binding : Binding.t -> 'acc -> 'acc =
        fun binding acc ->
          let { bound : computation; as_ : Name.t } = binding in
          self#computation bound acc |> self#name as_

      method string : string -> 'acc -> 'acc = fun _ acc -> acc

      method kind : Kind.t -> 'acc -> 'acc =
        fun kind acc ->
          match kind with
          | Bindings { bindings; last_body } ->
            List.fold ~init:acc bindings ~f:(Fn.flip self#binding)
            |> self#computation last_body
          | Value value -> self#value value acc
          | Wrapping { name; introduces; bodies } ->
            self#string name acc
            |> fun acc ->
            List.fold introduces ~init:acc ~f:(Fn.flip self#name)
            |> fun acc -> List.fold bodies ~init:acc ~f:(Fn.flip self#computation)

      method computation : Computation.t -> 'acc -> 'acc =
        fun computation acc ->
          let { kind : Kind.t
              ; free_variables : Name.Set.t
              ; here : Source_code_position.Stable.V1.t option
              }
            =
            computation
          in
          self#kind kind acc
          |> fun acc ->
          Set.fold free_variables ~init:acc ~f:(Fn.flip self#name)
          |> fun acc ->
          Option.value_map here ~f:(fun here -> self#position here acc) ~default:acc
    end
end
