open! Core
open! Bonsai_web
open Bonsai.Let_syntax

type result =
  { view : Vdom.Node.t
  ; out : float Int.Map.t
  ; reset : unit Effect.t
  }

module Model = struct
  type t = float Int.Map.t [@@deriving sexp, equal]
end

include Model

let max_size = 50
let total_space = max_size * 3 / 2

let rec generate_random () =
  let random = Splittable_random.State.create Random.State.default in
  let quickcheck_generator_color =
    let%map.Quickcheck.Generator i = Int.gen_uniform_incl 0 100 in
    Float.of_int i /. 100.0
  in
  let module Int = struct
    include Int

    let quickcheck_generator = Int.gen_uniform_incl 0 total_space
  end
  in
  let generator = [%quickcheck.generator: color Map.M(Int).t] in
  let res = Quickcheck.Generator.generate ~size:max_size ~random generator in
  let len = Map.length res in
  if len > max_size then generate_random () else res
;;

module Action = struct
  type t =
    | Regenerate
    | Remove of int
  [@@deriving sexp_of]
end

let component name =
  let%sub state, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)
      ~default_model:Int.Map.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Regenerate -> generate_random ()
        | Remove i -> Map.remove model i)
  in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map inject = inject in
         inject Regenerate)
      ()
  in
  let%arr state = state
  and inject = inject in
  let header =
    Vdom.Node.div
      ~attr:Style.header
      [ Style.refresh_button `Dark ~on_click:(inject Action.Regenerate)
      ; Vdom.Node.text name
      ]
  in
  let body =
    Vdom.Node.div
      ~attr:Style.body
      (List.map (Map.to_alist state) ~f:(fun (k, v) ->
         let color = Style.color_of_mult v in
         let border_color = Style.darker_color_of_mult v in
         Vdom.Node.div
           ~attr:
             (Vdom.Attr.many
                [ Style.row
                ; Vdom.Attr.style (Css_gen.background_color (`HSLA color))
                ; Vdom.Attr.style
                    (Css_gen.border
                       ~width:(`Px 3)
                       ~style:`Solid
                       ~color:(`HSLA border_color)
                       ())
                ])
           [ Vdom.Node.div
               ~attr:(Vdom.Attr.style (Css_gen.font_size (`Em_float 0.3)))
               [ Style.x_button `Light ~on_click:(inject (Action.Remove k)) ]
           ; Vdom.Node.textf "%d" k
           ]))
  in
  let view = Vdom.Node.div ~attr:Style.color_list [ header; body ] in
  { out = state; view; reset = inject Action.Regenerate }
;;
