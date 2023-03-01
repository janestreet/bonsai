open! Core
open! Bonsai_web
open! Js_of_ocaml
open! Bonsai.Let_syntax

module Input = struct
  type ('k, 'cmp) t =
    { children : ('k, Vdom.Node.t, 'cmp) Map.t
    ; tag : string
    ; attr : Vdom.Attr.t option
    }

  let to_empty_vdom { tag; attr; children = _ } = Vdom.Node.create tag ?attr []
end

let diff_patch ~prev ~next ~element =
  let diff = Vdom.Node.Patch.create ~previous:prev ~current:next in
  let (_ : Js_of_ocaml.Dom_html.element Js.t) = Vdom.Node.Patch.apply diff element in
  ()
;;

module Widget (K : Comparator.S) = struct
  type dom = Dom_html.element

  module State = struct
    type t =
      { elements : (Vdom.Node.t * Dom_html.element Js.t) Map.M(K).t
      ; me : dom Js.t
      }

    let sexp_of_t = sexp_of_opaque
  end

  let name = "vdom-node-with-map-children"

  (* This function diffs two vdom nodes.  It is notable for being happy to diff nodes that
     have different keys. *)
  let do_change ~key_before ~key_after ~elements current_vdom =
    let old_vdom, old_element = Map.find_exn elements key_before in
    let patch = Vdom.Node.Patch.create ~previous:old_vdom ~current:current_vdom in
    let current_element =
      (* if there isn't a patch, we can use the old element without issue *)
      if Vdom.Node.Patch.is_empty patch
      then old_element
      else Vdom.Node.Patch.apply patch old_element
    in
    let elements =
      let elements =
        (* This branch potentially saves a second tree traversal on the common
           case where both the before and after key are equal. *)
        match phys_equal key_before key_after with
        | true -> elements
        | false -> Map.remove elements key_before
      in
      Map.set elements ~key:key_after ~data:(current_vdom, current_element)
    in
    elements, current_element
  ;;

  module Acc = struct
    (* On every frame, we build an Acc.t, and keep track of which
       rows were added and removed ("changes" are just immediately diffed/patched without
       winding up in this structure).  After all added and removed nodes are visited,
       we "finalize" the structure by attempting to pair up rows to diff and re-insert. *)
    type t =
      { added : (K.t * Vdom.Node.t) list
      ; removed : (K.t * Vdom.Node.t * Dom_html.element Js.t) list
      ; state : State.t
      }

    let create ~state = { added = []; removed = []; state }

    let add t ~key ~vdom =
      let added = (key, vdom) :: t.added in
      { t with added }
    ;;

    let remove t ~key =
      let vdom, dom_node = Map.find_exn t.state.elements key in
      let removed = (key, vdom, dom_node) :: t.removed in
      { t with removed }
    ;;

    (* perform a reconciliation here becuase these vdom nodes share a key,
       so it's likely that they have similar vdom contents *)
    let change t ~key ~current_vdom =
      let elements, _new_element =
        do_change ~key_before:key ~key_after:key ~elements:t.state.elements current_vdom
      in
      { t with state = { t.state with elements } }
    ;;

    (* This empty div with a key is used to delete nodes by diffing/patching them against it.
       Because no other node will have this specific key, they'll always be removed.  The reason
       we do this instead of just deleting the node out of the dom is so that their widget and hook
       "destroy" functions are called.

       The "key" is a random guid to avoid collisions. *)
    let empty_node = Vdom.Node.div ~key:"4b75966c-60a2-46f5-a2bb-4939e2cb4c52" []

    let rec finalize_loop ({ added; removed; state } as t) ~to_re_insert =
      match added, removed with
      | [], [] -> t, to_re_insert
      | [], (key, _, _) :: removed ->
        (* we only have nodes to remove *)
        let elements, empty_element =
          do_change ~key_before:key ~key_after:key ~elements:state.elements empty_node
        in
        let elements = Map.remove elements key in
        Dom.removeChild state.me empty_element;
        finalize_loop
          { added = []; removed; state = { state with elements } }
          ~to_re_insert
      | (key, vdom) :: added, [] ->
        (* we only have nodes to insert *)
        let child = Vdom.Node.to_dom vdom in
        let elements = Map.set state.elements ~key ~data:(vdom, child) in
        let to_re_insert = Map.set to_re_insert ~key ~data:child in
        finalize_loop
          { added; removed = []; state = { state with elements } }
          ~to_re_insert
      | (key_after, current_vdom) :: added, (key_before, _, _dom) :: removed ->
        (* we have a node to remove and a node to add, diff them *)
        let elements, new_dom =
          do_change ~key_before ~key_after ~elements:state.elements current_vdom
        in
        let to_re_insert = Map.set to_re_insert ~key:key_after ~data:new_dom in
        finalize_loop { added; removed; state = { state with elements } } ~to_re_insert
    ;;

    let prepend (element : Dom.element Js.t) (node : Dom.node Js.t) : unit =
      (* NOTE: This is done because Js_of_ocaml does not have bindings
         to the prepend method yet.

         https://developer.mozilla.org/en-US/docs/Web/API/Element/prepend *)
      Js.Unsafe.meth_call element "prepend" [| Js.Unsafe.inject node |]
    ;;

    let finalize t =
      let { state; _ }, to_re_insert =
        finalize_loop t ~to_re_insert:(Map.empty (module K))
      in
      (* re-insertions are kept in a map so that they are visited in ascending order. *)
      Map.iteri to_re_insert ~f:(fun ~key ~data:node ->
        (* both [insertBefore] and [prepend] will move the existing element, if it exists
           in the DOM. *)
        match Map.closest_key state.elements `Less_than key with
        | Some (_k, (_vdom, prior)) ->
          state.me##insertBefore (node :> Dom.node Js.t) prior##.nextSibling
          |> (ignore : Dom.node Js.t -> unit)
        | None ->
          let me = (state.me :> Dom.element Js.t) in
          prepend me (node :> Dom.node Js.t));
      state
    ;;
  end

  let create input =
    let dom = Vdom.Node.to_dom (Input.to_empty_vdom input) in
    let elements =
      Map.map input.Input.children ~f:(fun vdom ->
        let element = Vdom.Node.to_dom vdom in
        dom##appendChild (element :> Dom.node Js.t) |> (ignore : Dom.node Js.t -> unit);
        vdom, element)
    in
    { State.elements; me = dom }, dom
  ;;

  let destroy ~prev_input ~state ~element =
    (* destroying this widget is accomplished by running through the whole
       map and removing each child individually. *)
    let acc =
      Map.fold
        prev_input.Input.children
        ~init:(Acc.create ~state)
        ~f:(fun ~key ~data:_ acc -> Acc.remove acc ~key)
    in
    let _state = Acc.finalize acc in
    (* If our previous input had a hook in its attr, then we need to patch in
       a version without the attrs in order to run the hook destruction logic. *)
    if Option.is_some prev_input.attr
    then
      diff_patch
        ~prev:(Input.to_empty_vdom prev_input)
        ~next:(Input.to_empty_vdom { prev_input with attr = None })
        ~element
  ;;

  let update ~prev_input ~input ~state ~element =
    if not (String.equal prev_input.Input.tag input.Input.tag)
    then (
      (* we can't do an in-place diff / patch on two elements that have different
         tags, so let's just obliterate the previous one and re-create *)
      destroy ~prev_input ~state ~element;
      create input)
    else (
      if not (Option.equal phys_equal prev_input.attr input.attr)
      then
        diff_patch
          ~prev:(Input.to_empty_vdom prev_input)
          ~next:(Input.to_empty_vdom input)
          ~element;
      let init = Acc.create ~state in
      let acc =
        Map.fold_symmetric_diff
          ~data_equal:phys_equal
          prev_input.Input.children
          input.Input.children
          ~init
          ~f:(fun acc -> function
            | key, `Left _vdom -> Acc.remove acc ~key
            | key, `Right vdom -> Acc.add acc ~key ~vdom
            | key, `Unequal (_old_vdom, current_vdom) -> Acc.change acc ~key ~current_vdom)
      in
      Acc.finalize acc, element)
  ;;

  let to_vdom_for_testing =
    `Custom
      (fun { Input.children; attr; tag } ->
         Vdom.Node.create tag ?attr (Map.data children))
  ;;

  module Input = struct
    type t = (K.t, K.comparator_witness) Input.t

    let sexp_of_t = sexp_of_opaque
  end
end

(* js-only: The instancer is a weak-map from comparator objects to a widget-implemented vdom
   function. This is so that each kind of map will get its own widget for safe diffing.

   The implementation of this function is quite scary, no doubt, but Carl claims that
   physical-equality of comparator objects is proof that the key and comparator_witness
   types are guaranteed to be equal *)
module Instancer : sig
  val get : map:('k, Vdom.Node.t, 'cmp) Map.t -> ('k, 'cmp) Input.t -> Vdom.Node.t
end = struct
  module Weak_map = Bonsai_web_ui_element_size_hooks.Expert.Weak_map

  let instances : (Obj.t, Obj.t) Weak_map.t = Weak_map.create ()

  let get (type k cmp) ~(map : (k, Vdom.Node.t, cmp) Map.t)
    : (k, cmp) Input.t -> Vdom.Node.t
    =
    let key = (Obj.repr : _ Core.Comparator.t -> Obj.t) (Map.comparator map) in
    match Weak_map.get instances key with
    | Some i -> (Obj.obj : Obj.t -> (k, cmp) Input.t -> Vdom.Node.t) i
    | None ->
      let module M =
        Widget (struct
          include (val Map.comparator_s map)
        end)
      in
      let f = unstage (Vdom.Node.widget_of_module (module M)) in
      Weak_map.set instances key ((Obj.repr : (_ Input.t -> Vdom.Node.t) -> Obj.t) f);
      f
  ;;
end

let make ~tag ?attr children =
  let f = Instancer.get ~map:children in
  f { Input.children; tag; attr }
;;
