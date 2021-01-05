# Incrementality

If you're coming from `Incr_dom`, the Bonsai API might surprise you.  The
`Incr.t` type that is pervasive throughout `Incr_dom` programs is (almost)
nowhere to be found!  However, if you dig into the internals of Bonsai, you'll
find that the same strategy that makes it possible to write incremental web
apps using the `Incr_dom` framework is the backbone of Bonsai.

Let's look at a concrete example: A Bonsai component that is built out of two
other Bonsai components and joins their results.

```ocaml
open Bonsai.Let_syntax

let super_component input = 
  let%sub c1 = component_1 input in
  let%sub c2 = component_2 input in
  return (
    let%map c1 = c1 
    and     c2 = c2 in 
    Vdom.Node.div [] [ c1; c2 ])
```

This Bonsai function approximately lowers to the following `Incr_dom` implementation:

```ocaml
module Model = struct 
  type t = 
    { c1_model : C1.Model.t
    ; c2_model : C2.Model.t
    } [@@deriving fields]
end

module Action = struct 
  type t = 
    | For_first_component of C1.Action.t
    | For_second_component of C2.Action.t
end

let super_component 
  ~input:(Input.t Incr.t)
  ~model:(Model.t Incr.t)
  ~old_model:(Model.t option Incr.t)
  ~inject:(Action.t -> Event.t)
  : (Action.t, 'Model.t, unit) Incr_dom.Component.t Incr.t =
  let c1_model = model >>| Model.c1_model in 
  let c2_model = model >>| Model.c2_model in 
  let c1_old_model = old_model >>| Option.map ~f:Model.c1_model in 
  let c2_old_model = old_model >>| Option.map ~f:Model.c2_model in 
  let c1_inject c1_a = inject (Action.For_first_component c1a) in 
  let c2_inject c2_a = inject (Action.For_second_component c2a) in 
  let c1_component = C1.create ~input ~model:c1_model ~old_model:c1_old_model ~inject:c1_inject in
  let c2_component = C2.create ~input ~model:c2_model ~old_model:c2_old_model ~inject:c2_inject in 
  let apply_action = 
    let%map c1_apply_action = c1_component >>| Component.apply_action
    and     c2_apply_action = c2_component >>| Component.apply_action
    and     model           = model in
    fun action ~schedule_action -> 
      let schedule_c1_action a1 = schedule_action (Action.For_first_component  a1) in 
      let schedule_c2_action a2 = schedule_action (Action.For_second_component a2) in 
      match action with 
      | Action.For_first_component  a1 -> 
        let new_c1_model = c1_apply_action a1 ~schedule_action:schedule_c1_action in 
        { model with c1_model = new_c1_model }
      | Action.For_second_component a2 -> 
        let new_c2_model = c2_apply_action a2 ~schedule_action:schedule_c2_action in 
        { model with c2_model = new_c2_model }
  in 
  let view = 
    let%map c1_view = c1_component >>| Component.view 
    and     c2_view = c2_component >>| Component.view in 
    Vdom.Node.div [] [ c1_view; c2_view ] 
  in 
  let%map apply_action = apply_action 
  and     view         = view 
  and     model        = model in 
  Component.create ~apply_action view model
;;
```

All those incremental `let%map`s in the `Incr_dom` code make it so that one
component's model changing doesn't force the other component to recompute.  The
same exact logic exists in the Bonsai code as well, it's just performed in the 
Bonsai library (specifically in the implementations of `Bonsai.Let_syntax.sub`
and `Bonsai.Let_syntax.both`).

Using Incremental nodes directly is possible in Bonsai, though it is not what
we would consider "typical" use of the library.  The `Bonsai.Arrow.With_incr` module
offers copies of many of the same functions in the regular `Bonsai` namespace, but 
with their incremental types exposed.  These functions are recommended only if you 
know that you can write an incremental function that is faster than the equivalent 
non-incremental code.
