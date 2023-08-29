open! Core
open! Bonsai_web
open Vdom
open Bonsai.Let_syntax
module Id = Int

module Ids = struct
  module State = struct
    type t =
      { next : Id.t
      ; ids : unit Id.Map.t
      }
    [@@deriving sexp, equal, fields ~getters]

    let default = { next = 0; ids = Id.Map.empty }
  end

  module Result = struct
    type t =
      { ids : unit Id.Map.t
      (** A unit map, not a set, to make it easier to plug into [Bonsai.assoc] *)
      ; inject_add_with_next_id : unit Ui_effect.t
      ; inject_remove : Id.t -> unit Ui_effect.t
      }
  end

  let apply_action (_ : _ Bonsai.Apply_action_context.t) (state : State.t) = function
    | `Remove x -> { state with ids = Map.remove state.ids x }
    | `Add_with_next_id ->
      { next = state.next + 1; ids = Map.add_exn ~key:state.next ~data:() state.ids }
  ;;

  let component =
    let%sub state =
      Bonsai.state_machine0
        ()
        ~sexp_of_model:[%sexp_of: State.t]
        ~equal:[%equal: State.t]
        ~sexp_of_action:[%sexp_of: [ `Remove of Id.t | `Add_with_next_id ]]
        ~default_model:State.default
        ~apply_action
    in
    let%arr state, inject = state in
    Result.
      { ids = State.ids state
      ; inject_remove = (fun x -> inject (`Remove x))
      ; inject_add_with_next_id = inject `Add_with_next_id
      }
  ;;
end

let panel_component id (_ : unit Value.t) =
  let%arr id = id in
  Node.div [ Node.textf !"Hello, world %{Id}!" id ]
;;

let component =
  let%sub { ids; inject_add_with_next_id; inject_remove } = Ids.component in
  let%sub panels = Bonsai.assoc (module Id) ids ~f:panel_component in
  Bonsai_web_ui_panels_experimental.component
    ~key:(module Id)
    ~inject_add:inject_add_with_next_id
    ~inject_remove
    panels
;;

let () = Bonsai_web.Start.start component
