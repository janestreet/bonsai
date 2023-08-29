open! Core
open! Import
open! Bonsai_web

type ('result, 'parsed) t =
  { value : 'result
  ; set : 'parsed -> unit Bonsai.Effect.t
  }
[@@deriving fields ~iterators:create]

let create = Fields.create

let lift { value; set } ~f =
  let set lifted_parsed_value = f lifted_parsed_value |> set in
  { value; set }
;;

module With_view = struct
  type 'a t =
    { value : 'a
    ; view : Vdom.Node.t
    }
  [@@deriving fields ~iterators:create]

  let create = Fields.create
end

module Same = struct
  type nonrec 'a t = ('a With_view.t, 'a) t
end

module Errorable = struct
  type nonrec ('result, 'parsed) t = ('result Or_error.t With_view.t, 'parsed) t

  module Same = struct
    type nonrec 'a t = ('a, 'a) t
  end
end
