# Incrementality

If you're coming from `Incr_dom`, the Bonsai API might suprise you.  The
`Incr.t` type that is pervasive throughout `Incr_dom` programs is (almost)
nowhere to be found!  However, if you dig into the internals of Bonsai, you'll
find that the same strategy that makes it possible to write incremental web
apps using the `Incr_dom` framework is the backbone of Bonsai.

The reason that Bonsai can get away with not exposing Incremental
nodes to the user is that Bonsai is built to encourage users to build
small UI components and then to compose them using its array of
projections and combinators.  When these functions are used, Bonsai
adds incrementality under the hood for you.

Let's look at a concrete example: A Bonsai component that is built out of two
other Bonsai components and joins their results.

```ocaml
module Input = struct ... end
module Model = struct 
  type t = 
    { c1_model : C1.Model.t
    ; c2_model : C2.Model.t
    } [@@deriving fields]
end

let super_component : (Input.t, Model.t, Vdom.Node.t) Bonsai.t = 
  let%map c1 = component_1 |> Bonsai.Project.Model.field Model.Fields.c1_model 
  and     c2 = component_2 |> Bonsai.Project.Model.field Model.Fields.c2_model 
  in Vdom.Node.div [] [ c1; c2 ]
```

Because we used the Bonsai let-syntax, the components that are bound are passed
to `Bonsai.Let_syntax.both`, which is implemented as an incremental function
that takes the two components and returns a new one.

The `Incr_dom` structure that this Bonsai function approximately lowers to is
presented here for the readers that are familiar with `Incr_dom`
componentization idioms:

```ocaml
module Input = struct ... end
module Model = struct 
  type t = 
    { c1_model : C1.Model.t
    ; c2_model : C2.Model.t
    } [@@deriving fields]
end

let super_component 
  ~input:(Input.t Incr.t)
  ~model:(Model.t Incr.t)
  ~old_model:(Model.t option Incr.t)
  ~inject:((C1.Action.t, C2.Action.t) Either.t -> Event.t) =
  let c1_model = model >>| Model.c1_model in 
  let c2_model = model >>| Model.c2_model in 
  let c1_old_model = old_model >>| Option.map ~f:Model.c1_model in 
  let c2_old_model = old_model >>| Option.map ~f:Model.c2_model in 
  let c1_inject c1_a = inject (Either.First  c1a) in 
  let c2_inject c2_a = inject (Either.Second c2a) in 
  let c1_component = C1.create ~input ~model:c1_model ~old_model:c1_old_model ~inject:c1_inject in
  let c2_component = C2.create ~input ~model:c2_model ~old_model:c2_old_model ~inject:c2_inject in 
  let apply_action = 
    let%map c1_apply_action = c1_component >>| Component.apply_action
    and     c2_apply_action = c2_component >>| Component.apply_action
    and     model           = model in
    fun action ~schedule_action -> 
      let schedule_c1_action a1 = schedule_action (Either.First  a1) in 
      let schedule_c2_action a2 = schedule_action (Either.Second a2) in 
      match action with 
      | Either.First  a1 -> 
        let new_c1_model = c1_apply_action a1 ~schedule_action:schedule_c1_action in 
        { model with c1_model = new_c1_model }
      | Either.Second a2 -> 
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
Bonsai library (specifically in the implementation of `Bonsai.Let_syntax.both`).

Using Incremental nodes directly is possible in Bonsai, though it is not what
we would consider "typical" use of the library.  The `Bonsai.Incremental` library 
offers copies of many of the same functions in the regular `Bonsai` namespace, but 
with their incremental types exposed.  These functions are recommended only if you 
know that you can write an incremental function that is faster than the equivalent 
non-incremental code.

Another option is to use the `Bonsai.Expert` interface which directly exposes
Bonsai's incremental guts.
