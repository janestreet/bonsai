open! Core
open! Bonsai_web
open! Js_of_ocaml
open Bonsai.Let_syntax
open Virtual_dom

module Low_level = struct
  module Id = Unique_id.Int ()

  let zero = Id.create ()

  type 's t =
    { unsafe_init : 's -> Id.t
    ; unsafe_destroy : Id.t -> unit
    ; modify : ('s -> unit) -> unit Effect.t
    ; read : 'a. ('s -> 'a) -> 'a list Effect.t
    }

  module Model = struct
    type 's t = 's Id.Map.t

    let sexp_of_t = sexp_of_opaque
    let t_of_sexp = opaque_of_sexp
    let equal = phys_equal
  end

  module Action = struct
    type 's t =
      | Register of
          { id : Id.t
          ; state : 's
          }
      | Destroy of Id.t
      | Modify of ('s -> unit)

    let sexp_of_t = sexp_of_opaque
  end

  let component (type s) () =
    let module Model = struct
      include Model

      type nonrec t = s t
    end
    in
    let module Action = struct
      include Action

      type nonrec t = s t
    end
    in
    let%sub model, inject =
      Bonsai.state_machine0
        (module Model)
        (module Action)
        ~reset:(fun ~inject:_ ~schedule_event:_ m -> m)
        ~default_model:Id.Map.empty
        ~apply_action:(fun ~inject:_ ~schedule_event:_ model -> function
          | Register { id; state } -> Map.set model ~key:id ~data:state
          | Destroy id -> Map.remove model id
          | Modify f ->
            Map.iter model ~f;
            model)
    in
    let%sub get_model = Bonsai.yoink model in
    let%arr inject = inject
    and get_model = get_model in
    let unsafe_init state =
      let id = Id.create () in
      Effect.Expert.handle_non_dom_event_exn (inject (Register { id; state }));
      id
    in
    let unsafe_destroy id =
      Effect.Expert.handle_non_dom_event_exn (inject (Destroy id))
    in
    let modify f = inject (Modify f) in
    let read r =
      match%map.Effect get_model with
      | Inactive -> []
      | Active m -> List.map (Map.data m) ~f:r
    in
    { unsafe_init; unsafe_destroy; modify; read }
  ;;
end

module State = struct
  type ('input, 'state) t =
    { mutable input : 'input
    ; mutable state : 'state
    ; mutable id : Low_level.Id.t
    ; get_input : unit -> 'input
    }
end

type ('input, 'state) reader = { f : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t }

type ('input, 'state) t =
  { view : Vdom.Node.t
  ; modify : ('input -> 'state -> unit) -> unit Effect.t
  ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
  }

module type S = sig
  type element = private #Dom_html.element
  type input
  type state

  val init : get_input:(unit -> input) -> input -> state * element Js.t
  val update : prev_input:input -> input -> state -> element Js.t -> element Js.t
  val destroy : input -> state -> element Js.t -> unit
end

let component
      (type input state)
      ?(vdom_for_testing = fun _ -> Vdom.Node.create "widget" [])
      (module M : S with type input = input and type state = state)
      input
  =
  let id = Type_equal.Id.create ~name:"widget" sexp_of_opaque in
  let%sub state_tracker = Low_level.component () in
  let%sub view =
    let%arr input = input
    and { unsafe_init; unsafe_destroy; _ } = state_tracker in
    Vdom.Node.widget
      ~vdom_for_testing:(lazy (vdom_for_testing input))
      ~id
      ~init:(fun () ->
        let the_state = ref None in
        let get_input () =
          match !the_state with
          | None -> input
          | Some s -> s.State.input
        in
        let state, element = M.init ~get_input input in
        let s = { State.input; state; id = Low_level.zero; get_input } in
        the_state := Some s;
        let id = unsafe_init s in
        s.id <- id;
        s, element)
      ~update:(fun s element ->
        let { State.input = prev_input; state; get_input = _; id = _ } = s in
        if phys_equal input prev_input
        then s, element
        else (
          s.input <- input;
          let element = M.update ~prev_input input state element in
          s, element))
      ~destroy:(fun s element ->
        let { State.input; state; id; get_input = _ } = s in
        unsafe_destroy id;
        M.destroy input state element)
      ()
  in
  let%sub funs =
    let%arr state_tracker = state_tracker in
    let modify f = state_tracker.modify (fun s -> f s.input s.state) in
    let reader = { f = (fun f -> state_tracker.read (fun s -> f s.input s.state)) } in
    modify, reader
  in
  let%arr view = view
  and modify, reader = funs in
  { view; modify; read = reader.f }
;;
