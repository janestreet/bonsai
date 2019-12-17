"`Bonsai`: A library for writing reusable UI components"
========================================================

Bonsai is a library for building reusable UI components inside an
Incremental-style UI framework such as Incr_dom.  The API is broken
up into roughly two concepts. 

1. Creating components
2. Combining components


# Components

Before diving into those modules, let's first examine what a
"Component" is.

The component type (`('model, 'result) Bonsai.t`) can be thought of as
a function from `'model` to `'result`, but where the `'result` can
schedule events of type `'action`. These `'action`s are used to
produce a new `'model` which in turn causes the `'result` to be
recomputed. Instances of the `'result` type can contain callbacks
which schedule `'action`s when interacted with by user (via button
click, text input, etc). These actions are handled by the components
`apply_action` function, which yields a new model to be displayed.

The basic life-cycle of a component is illustrated below.

```svgbob
                 .----------------.     compute function
                 |     'model     |-------.
                 '----------------'       |
                    ^                     v
                    |              .-------------.
new model produced  |              |   'result   |
                    |              '-------------'
                    |                     |
               .----------------.         |
               |  apply_action  |<--------'
               '----------------'   'action emitted
```

# Constructing Components

## `Bonsai.of_module`

The most complete component constructor is the `Bonsai.of_module`
function which takes a first-class module with both the `compute`
function and `apply_action` function.

Here's an example of an Bonsai_web component:

```ocaml
module Counter_component = struct
  module Result = Vdom.Node
  module Model = Int

  module Action = struct
    type t =
      | Increment
      | Decrement
    [@@deriving sexp_of]
  end

  let apply_action ~inject:_ ~schedule_event:_ model = function
    | Action.Increment -> model + 1
    | Action.Decrement -> model - 1
  ;;

  let compute ~inject model =
    let button label action =
      let on_click = Vdom.Attr.on_click (fun _ -> inject action) in
      Vdom.Node.button [ on_click ] [ Vdom.Node.text label ]
    in
    Vdom.Node.div
      []
      [ button "-1" Action.Decrement
      ; Vdom.Node.text (Int.to_string model)
      ; button "+1" Action.Increment
      ]
  ;;
end

let counter_component
  : ( Counter_component.Result.t, Counter_component.Model.t) Bonsai.t
  = Bonsai.of_module (module Counter_component)
;;
```

Aside from defining the result, model, and action types, this component declares
its result `compute` function. This component also defines the `apply_action`
callback which looks at an action and creates a new model based off that action.

## `Bonsai.of_fn`

For components that produce no action, `Bonsai.of_fn` takes a standard function
`'model -> 'result` and produces a component of type `('model, 'result) Bonsai.t`.

```ocaml
let rational_component : (string, float * float) Bonsai.t
  = Bonsai.of_fn (fun (numerator, denominator) ->
    sprintf "%f / %f" numerator denominator)
```

## `Bonsai.const`

`Bonsai.const` is used to produce a component whose result never changes.
It has a model of type `unit`.

```ocaml
let constant_component: (unit, string) Bonsai.t = Component.const "hello world"
```
