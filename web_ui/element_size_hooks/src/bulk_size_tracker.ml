open! Core
open! Import
open! Bonsai_web
open! Js_of_ocaml

module Dimensions = struct
  type t =
    { width : float
    ; height : float
    }
  [@@deriving sexp, equal]
end

module Action = struct
  type 'a item =
    | Set of 'a * Dimensions.t
    | Remove of 'a
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

let weakmap : (Dom.node Js.t, Input.t) Weak_map.t = Weak_map.create ()

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

    (* This module is necessary because we need to merge the map of actions
       with the map of trackers to apply in them. The result will not be
       polymorphic, but output module of Univ_map.Merge is expected to be
       polymorphic. *)
    module Poly_unit_effect = struct
      type 'a t = unit Effect.t

      let sexp_of_t _ = sexp_of_opaque
    end

    module Apply_trackers =
      Univ_map.Merge (Group_key) (Tracker) (Action) (Poly_unit_effect)

    module Collected_effects = Univ_map.Make (Group_key) (Poly_unit_effect)

    let change_sizes changes =
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

    let observer () =
      let on_resize_observed entries _observer =
        let changes = ref Reversed_list.[] in
        for i = 0 to entries##.length - 1 do
          let open Option.Let_syntax in
          (ignore : unit option -> unit)
          @@ let%bind entry = Js.array_get entries i |> Js.Optdef.to_option in
          let%bind border_box =
            Js.array_get entry##.borderBoxSize 0 |> Js.Optdef.to_option
          in
          let target = entry##.target in
          let%map input = Weak_map.get weakmap target in
          let width = border_box##.inlineSize in
          let height = border_box##.blockSize in
          let dimensions = { Dimensions.width; height } in
          changes := Reversed_list.((input, dimensions) :: !changes)
        done;
        change_sizes (Reversed_list.rev !changes)
      in
      new%js ResizeObserver.resizeObserver (Js.wrap_callback on_resize_observed)
    ;;

    let observer = lazy (observer ())
    let init _ _ = ()

    let on_mount input () element =
      Weak_map.set weakmap (element :> Dom.node Js.t) input;
      (Lazy.force observer)##observe element
    ;;

    (* This update function is unsound if the injection function passed
       to the hook ever changes. For the time being, this is fine
       because the injection function is built by the component, and we know
       that it never changes. *)
    let update ~old_input:_ ~new_input:_ () _ = ()

    let destroy old_input _ element =
      Weak_map.delete weakmap (element :> Dom.node Js.t);
      (Lazy.force observer)##unobserve element;
      List.iter (Input.to_alist old_input) ~f:(fun (Input.Packed.T (group_key, keys)) ->
        let tracker = Trackers.find_exn !Trackers.the_one_and_only group_key in
        List.iter keys ~f:(fun key ->
          Vdom.Effect.Expert.handle_non_dom_event_exn (tracker [ Remove key ])))
    ;;
  end

  include T
  include Vdom.Attr.Hooks.Make (T)
end

module Options = struct
  type 'a maybe_stale =
    | Fresh of 'a
    | Stale of 'a
  [@@deriving sexp, equal]

  type 'a t =
    | Prune_stale : Dimensions.t t
    | Keep_stale : Dimensions.t maybe_stale t
    | Ignore_stale : Dimensions.t t
end

let component
      (type key cmp contained)
      (key : (key, cmp) Bonsai.comparator)
      (options : contained Options.t)
  =
  let open Bonsai.Let_syntax in
  let module Key = (val key) in
  let on_change ~group_key ~key =
    Vdom.Attr.many
      [ Vdom.Attr.create_hook
          "bulk_size_tracker"
          (Hook.create (Input.singleton group_key [ key ]))
      ; Vdom.Attr.style (Css_gen.box_sizing `Border_box)
      ]
  in
  let module Model = struct
    let sexp_of_contained : contained -> Sexp.t =
      match options with
      | Prune_stale -> [%sexp_of: Dimensions.t]
      | Keep_stale -> [%sexp_of: Dimensions.t Options.maybe_stale]
      | Ignore_stale -> [%sexp_of: Dimensions.t]
    ;;

    let contained_of_sexp : Sexp.t -> contained =
      match options with
      | Prune_stale -> [%of_sexp: Dimensions.t]
      | Keep_stale -> [%of_sexp: Dimensions.t Options.maybe_stale]
      | Ignore_stale -> [%of_sexp: Dimensions.t]
    ;;

    let equal_contained : contained -> contained -> bool =
      match options with
      | Prune_stale -> [%equal: Dimensions.t]
      | Keep_stale -> [%equal: Dimensions.t Options.maybe_stale]
      | Ignore_stale -> [%equal: Dimensions.t]
    ;;

    type t = contained Map.M(Key).t [@@deriving sexp, equal]
  end
  in
  let%sub sizes, inject =
    Bonsai.state_machine0
      (module Model)
      (module struct
        type t = Key.t Action.t [@@deriving sexp_of]
      end)
      ~default_model:(Map.empty (module Key))
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model actions ->
        List.fold actions ~init:model ~f:(fun model action ->
          match action, options with
          | Set (k, v), Prune_stale -> Map.set model ~key:k ~data:v
          | Set (k, v), Ignore_stale -> Map.set model ~key:k ~data:v
          | Set (k, v), Keep_stale -> Map.set model ~key:k ~data:(Fresh v)
          | Remove k, Prune_stale -> Map.remove model k
          | Remove k, Ignore_stale ->
            eprint_s
              [%message
                "BUG: We should never be removing items from this map while in \
                 [Ignore_stale] mode. Removing anyway."];
            Map.remove model k
          | Remove k, Keep_stale ->
            Map.change model k ~f:(function
              | None -> None
              | Some (Stale v | Fresh v) -> Some (Stale v))))
    |> Bonsai.Incr.model_cutoff
  in
  let%sub group_key =
    Bonsai.Expert.thunk (fun () ->
      Type_equal.Id.create ~name:"bulk_size_tracker_type_id" [%sexp_of: Key.t])
  in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map inject = inject
         and group_key = group_key in
         let inject =
           match options with
           | Ignore_stale ->
             fun actions ->
               (match
                  List.filter actions ~f:(function
                    | Action.Remove _ -> false
                    | _ -> true)
                with
                | [] -> Effect.Ignore
                | actions -> inject actions)
           | _ -> inject
         in
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
  return @@ Value.both sizes attr
;;

module For_testing = struct
  type t = Hook.Input.t

  let type_id = Hook.For_testing.type_id
  let hook_name = "bulk_size_tracker"
  let change_sizes = Hook.change_sizes
end
