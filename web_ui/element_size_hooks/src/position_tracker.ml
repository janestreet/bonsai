open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

module Position = struct
  type t =
    { top : int
    ; left : int
    ; height : int
    ; width : int
    }
  [@@deriving sexp, equal]
end

module Position_tracker = struct
  type ('key, 'cmp) t =
    { positions : ('key, Position.t, 'cmp) Base.Map.t
    ; get_attr : 'key -> Vdom.Attr.t
    ; update : unit Effect.t
    }
end

module Action = struct
  type 'a item =
    | Set of 'a * Position.t
    | Remove of 'a
    | Observe of (('a * Dom_html.element Js.t)[@sexp.opaque])
    | Unobserve of ('a[@sexp.opaque])
  [@@deriving sexp_of]

  type 'a t = 'a item list [@@deriving sexp_of]
end

module Group_key = Univ_map.Type_id_key
module Input = Univ_map.Make (Group_key) (List)

module Tracker = struct
  type 'a t = 'a Action.t -> unit Effect.t [@@deriving sexp_of]
end

module Trackers = struct
  include Univ_map.Make (Group_key) (Tracker)

  let the_one_and_only : t ref = ref empty

  let set (inject, group_key) =
    the_one_and_only := set !the_one_and_only ~key:group_key ~data:inject
  ;;

  let set ~inject ~group_key = (Effect.of_sync_fun set) (inject, group_key)
  let remove group_key = the_one_and_only := remove !the_one_and_only group_key
  let remove ~group_key = (Effect.of_sync_fun remove) group_key
end

module Event_groups = Univ_map.Make (Group_key) (Action)

module Hook = struct
  module T = struct
    module Input = struct
      include Input
      module Merge = Univ_map.Merge (Group_key) (List) (List) (List)

      let combine left right =
        let f ~key:_ = function
          | `Both (left, right) -> Some (left @ right)
          | `Left x | `Right x -> Some x
        in
        Merge.merge left right ~f:{ f }
      ;;
    end

    module State = Unit

    module Poly_unit_effect = struct
      type 'a t = unit Effect.t

      let sexp_of_t _ = sexp_of_opaque
    end

    module Apply_trackers =
      Univ_map.Merge (Group_key) (Tracker) (Action) (Poly_unit_effect)

    module Collected_effects = Univ_map.Make (Group_key) (Poly_unit_effect)

    let change_positions changes =
      let dimension_groups = ref Event_groups.empty in
      List.iter changes ~f:(fun (input, dimensions) ->
        Input.to_alist input
        |> List.iter ~f:(fun (T (group_key, keys)) ->
          List.iter keys ~f:(fun key ->
            dimension_groups
            := Event_groups.change !dimension_groups group_key ~f:(fun items ->
              let items = Option.value ~default:[] items in
              Some (Action.Set (key, dimensions) :: items)))));
      let events =
        let f ~key:_ = function
          | `Both (tracker, dimension_group) -> Some (tracker dimension_group)
          | `Left _ ->
            (* Some trackers might not have changes, so they don't need to
               generate an event *)
            None
          | `Right _ -> assert false
        in
        Apply_trackers.merge !Trackers.the_one_and_only !dimension_groups ~f:{ f }
        |> Collected_effects.to_alist
        |> List.map ~f:(fun (T (_, effect)) -> effect)
      in
      Ui_effect.Expert.handle (Ui_effect.Many events)
    ;;

    let init _ _ = ()

    let on_mount input () element =
      let (T (group_key, key)) = List.hd_exn (Input.to_alist input) in
      let key = List.hd_exn key in
      Option.iter (Trackers.find !Trackers.the_one_and_only group_key) ~f:(fun tracker ->
        Vdom.Effect.Expert.handle_non_dom_event_exn (tracker [ Observe (key, element) ]))
    ;;

    let update ~old_input:_ ~new_input:_ () _ = ()

    let destroy old_input _ _ =
      List.iter (Input.to_alist old_input) ~f:(fun (Input.Packed.T (group_key, keys)) ->
        let tracker = Trackers.find_exn !Trackers.the_one_and_only group_key in
        List.iter keys ~f:(fun key ->
          Vdom.Effect.Expert.handle_non_dom_event_exn
            (tracker [ Remove key; Unobserve key ])))
    ;;
  end

  include T
  include Vdom.Attr.Hooks.Make (T)
end

let component (type key cmp) (key : (key, cmp) Bonsai.comparator) =
  let module Key = (val key) in
  let on_change ~group_key ~key =
    Vdom.Attr.many
      [ Vdom.Attr.create_hook
          "bulk_position_tracker"
          (Hook.create (Input.singleton group_key [ key ]))
      ; Vdom.Attr.style (Css_gen.box_sizing `Border_box)
      ]
  in
  let module Model = struct
    module Dom_element = struct
      type t = Dom_html.element Js.t

      let equal = phys_equal
      let sexp_of_t = sexp_of_opaque
    end

    type t =
      { position_tracker : Position.t Map.M(Key).t
      ; dom_node_tracker : Dom_element.t Map.M(Key).t
      }
    [@@deriving sexp_of, equal]

    let empty =
      { position_tracker = Map.empty (module Key)
      ; dom_node_tracker = Map.empty (module Key)
      }
    ;;

    let dom_element_to_position (element : Dom_html.element Js.t) : Position.t =
      let left = element##.offsetLeft in
      let top = element##.offsetTop in
      let height = element##.offsetHeight in
      let width = element##.offsetWidth in
      { Position.left; top; height; width }
    ;;

    let set old_model key position =
      let position_tracker = Map.set old_model.position_tracker ~key ~data:position in
      { old_model with position_tracker }
    ;;

    let remove old_model key =
      let position_tracker = Map.remove old_model.position_tracker key in
      { old_model with position_tracker }
    ;;

    let unobserve old_model key =
      let dom_node_tracker = Map.remove old_model.dom_node_tracker key in
      { old_model with dom_node_tracker }
    ;;

    let observe old_model key dom_node =
      let dom_node_tracker = Map.set old_model.dom_node_tracker ~key ~data:dom_node in
      { old_model with dom_node_tracker }
    ;;

    let apply_action
          ~(inject : key Action.t -> unit Effect.t)
          ~(schedule_event : unit Effect.t -> unit)
          old_model
          action
      =
      List.fold action ~init:old_model ~f:(fun acc -> function
        | Action.Set (key, position) -> set acc key position
        | Remove key -> remove acc key
        | Observe (key, dom_node) ->
          schedule_event (inject [ Action.Set (key, dom_element_to_position dom_node) ]);
          observe acc key dom_node
        | Unobserve key -> unobserve acc key)
    ;;
  end
  in
  let%sub mapping, apply_action =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Key.t Action.t]
      ~default_model:Model.empty
      ~apply_action:Model.apply_action
  in
  let%sub { position_tracker; dom_node_tracker } = return mapping in
  let%sub update =
    let update_f =
      Effect.of_sync_fun (fun dom_node_tracker ->
        Map.fold ~init:[] dom_node_tracker ~f:(fun ~key ~data acc ->
          let new_effect = Action.Set (key, Model.dom_element_to_position data) in
          new_effect :: acc)
        |> List.rev)
    in
    let%arr apply_action = apply_action
    and dom_node_tracker = dom_node_tracker in
    let%bind.Effect effects = update_f dom_node_tracker in
    apply_action effects
  in
  let%sub group_key =
    Bonsai.Expert.thunk (fun () ->
      Type_equal.Id.create ~name:"bulk_position_tracker_type_id" [%sexp_of: Key.t])
  in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map inject = apply_action
         and group_key = group_key in
         Trackers.set ~inject ~group_key)
      ~on_deactivate:
        (let%map group_key = group_key in
         Trackers.remove ~group_key)
      ()
  in
  let%sub attr =
    let%arr group_key = group_key in
    fun key -> on_change ~group_key ~key
  in
  let%arr position_tracker = position_tracker
  and attr = attr
  and update = update in
  { Position_tracker.positions = position_tracker; get_attr = attr; update }
;;

module For_testing = struct
  type t = Hook.Input.t

  let type_id = Hook.For_testing.type_id
  let hook_name = "bulk_position_tracker"
  let change_positions = Hook.change_positions
end
