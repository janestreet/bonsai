open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let mirror'
  (type m)
  ?sexp_of_model
  ~equal
  ~(store_set : (m -> unit Effect.t) Bonsai.t)
  ~(store_value : m option Bonsai.t)
  ~(interactive_set : (m -> unit Effect.t) Bonsai.t)
  ~(interactive_value : m option Bonsai.t)
  (local_ graph)
  =
  let module M = struct
    type t = m

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_model
  end
  in
  let module M2 = struct
    type model = M.t

    let equal_model = equal
    let sexp_of_model = M.sexp_of_t

    type t =
      { store : model option
      ; interactive : model option
      }
    [@@deriving sexp_of, equal]
  end
  in
  let callback =
    let%map store_set and interactive_set in
    fun old_pair { M2.store = store_value; interactive = interactive_value } ->
      let stability =
        if Option.equal equal store_value interactive_value then `Stable else `Unstable
      in
      match stability with
      | `Stable ->
        (* if both of the new values are the same, then we're done! Stability has already
           been reached. *)
        Effect.Ignore
      | `Unstable ->
        (match old_pair with
         | None ->
           (* on_change' is triggered when the values flow through this node for the first
              time. In this scenario, we prioritize the value in the store. *)
           (match store_value, interactive_value with
            | Some store_value, _ -> interactive_set store_value
            | None, Some interactive_value -> store_set interactive_value
            | None, None ->
              eprint_s
                [%message
                  "BUG" [%here] {|if both are None, then we shouldn't be `Unstable |}];
              Effect.Ignore)
         | Some { M2.store = old_store_value; interactive = old_interactive_value } ->
           let store_changed = not (Option.equal equal old_store_value store_value) in
           let interactive_changed =
             not (Option.equal equal old_interactive_value interactive_value)
           in
           (match interactive_changed, store_changed with
            (* if both the interactive-value and store values have changed, first try to
               forward it on to the store, but if the interactive value was changed to
               None and the store value was changed to a Some, then the interactive value
               gets set to the new store value. *)
            | true, true ->
              (match interactive_value, store_value with
               | Some interactive_value, (Some _ | None) -> store_set interactive_value
               | None, Some store_value -> interactive_set store_value
               | None, None -> Effect.Ignore)
            (*=when the interactive value changed, but the store did not, set the store to
               the new interactive value (if it's Some]. *)
            | true, false ->
              (match interactive_value with
               | Some interactive_value -> store_set interactive_value
               | None -> Effect.Ignore)
            (* finally, if the store changed but interactive did not, update the
               interactive value. *)
            | false, true ->
              (match store_value with
               | Some store_value -> interactive_set store_value
               | None -> Effect.Ignore)
            (* this final case should never happen. Error message explains why. *)
            | false, false ->
              eprint_s
                [%message
                  "BUG" [%here] "on_change triggered when nothing actually changed?"];
              Effect.Ignore))
  in
  Bonsai.Edge.on_change'
    ~trigger:`After_display
    ~sexp_of_model:[%sexp_of: M2.t]
    ~equal:[%equal: M2.t]
    (let%map store = store_value
     and interactive = interactive_value in
     { M2.store; interactive })
    ~callback
    graph
;;

let mirror
  ?sexp_of_model
  ~equal
  ~store_set
  ~store_value
  ~interactive_set
  ~interactive_value
  (local_ graph)
  =
  let store_value = store_value >>| Option.some in
  let interactive_value = interactive_value >>| Option.some in
  mirror'
    ?sexp_of_model
    ~equal
    ~store_set
    ~store_value
    ~interactive_set
    ~interactive_value
    graph
;;
