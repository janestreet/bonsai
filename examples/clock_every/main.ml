open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

module Css =
  [%css.raw
    {|
html, body {
  font-family: 'Open Sans', sans-serif;
}

.paper {
  box-shadow: 0 0 8px rgba(0,0,0,0.2);
  padding: 12px;
  border-radius: 4px;
  margin: 8px;
  max-width: fit-content;
  text-align: center;
}

.timeline_frame {
  box-shadow: 0 0 8px rgba(0,0,0,0.2);
  padding: 12px;
  border-radius: 4px;
  margin: 8px;
  width: 230px;
  height: 100px;
  background-color: black;
}

.row {
  display: flex;
  flex-direction: row;
  align-items: center;
  align-content: center;
}

.column {
  display: flex;
  flex-direction: column;
  align-items: center;
  align-content: center;
}

:root {
  box-sizing: border-box;
}
|}]

let paper = Vdom.Attr.class_ Css.paper
let column = Vdom.Attr.class_ Css.column
let row = Vdom.Attr.class_ Css.row
let timeline_frame = Vdom.Attr.class_ Css.timeline_frame

let colors =
  [| "#ff355e"
   ; "#fd5b78"
   ; "#ff6037"
   ; "#ff9966"
   ; "#ff9933"
   ; "#ffcc33"
   ; "#ffff66"
   ; "#ccff00"
   ; "#66ff66"
   ; "#aaf0d1"
   ; "#50bfe6"
   ; "#ff6eff"
   ; "#ee34d2"
   ; "#ff00cc"
  |]
;;

module Random_time_span = struct
  type t =
    { base_duration : Time_ns.Span.t
    ; extra_duration : Time_ns.Span.t
    ; chance_of_getting_extra_duration : float
    }
  [@@deriving typed_fields, sexp, equal]

  let default_base_duration = 0.5
  let default_extra_duration = 1.0
  let default_chance_of_getting_extra_duration = 0.5

  let form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let time_span_form ~max ~default =
          let%sub form = Form.Elements.Range.float ~min:0.0 ~max ~step:0.1 ~default () in
          let%arr form = form in
          Form.project form ~parse_exn:Time_ns.Span.of_sec ~unparse:Time_ns.Span.to_sec
        ;;

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | Base_duration -> time_span_form ~max:1.0 ~default:default_base_duration
          | Extra_duration -> time_span_form ~max:2.0 ~default:default_extra_duration
          | Chance_of_getting_extra_duration ->
            Form.Elements.Range.float
              ~min:0.0
              ~max:1.0
              ~step:0.1
              ~default:default_chance_of_getting_extra_duration
              ()
        ;;

        let label_for_field = `Inferred
      end)
  ;;

  let default =
    { base_duration = Time_ns.Span.of_sec default_base_duration
    ; extra_duration = Time_ns.Span.of_sec default_extra_duration
    ; chance_of_getting_extra_duration = default_chance_of_getting_extra_duration
    }
  ;;

  let get_wait_time t =
    if Float.(Random.float_range 0.0 1.0 < t.chance_of_getting_extra_duration)
    then Time_ns.Span.(t.base_duration + t.extra_duration)
    else t.base_duration
  ;;
end

module Bar_id = Bonsai_extra.Id_gen (Int) ()

let bar_id_to_color x = colors.(Bar_id.to_int_exn x % Array.length colors)

module Bar = struct
  type t =
    { start_time : Time_ns.Alternate_sexp.t
    ; end_time : Time_ns.Alternate_sexp.t option
    ; id : Bar_id.t
    }
  [@@deriving sexp, equal]

  let create time bar_id = { start_time = time; id = bar_id; end_time = None }
  let finish t ~now = { t with end_time = Some now }
end

module Track_id : sig
  type t [@@deriving sexp, equal]

  val of_int : int -> t
  val to_int : t -> int

  include Comparable with type t := t
end =
  Int

module Tracks = struct
  type t =
    { tracks : Bar.t Fdeque.t Track_id.Map.t
    ; track_ids_by_bar_id : Track_id.t Bar_id.Map.t
    ; last_trigger_time : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, equal]

  let empty =
    let tracks = Track_id.Map.empty in
    let track_ids_by_bar_id = Bar_id.Map.empty in
    let last_trigger_time = Time_ns.epoch in
    { tracks; track_ids_by_bar_id; last_trigger_time }
  ;;

  let find_lowest_clear_track t =
    Map.fold
      (t.tracks : _ Track_id.Map.t)
      ~init:None
      ~f:(fun ~key ~data acc ->
        match acc with
        | Some _ -> acc
        | None ->
          let first = Fdeque.peek_front data in
          (match first with
           | None -> Some key
           | Some x -> if Option.is_some x.end_time then Some key else None))
  ;;

  let add_bar t ~now bar_id =
    let next_lowest_clear_track = find_lowest_clear_track t in
    let track_id =
      match next_lowest_clear_track with
      | Some x -> x
      | None -> Map.length (t.tracks : _ Track_id.Map.t) |> Track_id.of_int
    in
    let remove_id, tracks =
      let remove_id, queue =
        let queue =
          Option.value
            (Map.find (t.tracks : _ Track_id.Map.t) track_id)
            ~default:Fdeque.empty
        in
        let queue = Fdeque.enqueue_front queue (Bar.create now bar_id) in
        if Fdeque.length queue >= 10
        then (
          let remove, queue = Fdeque.dequeue_back_exn queue in
          Some remove.id, queue)
        else None, queue
      in
      remove_id, Map.set (t.tracks : _ Track_id.Map.t) ~key:track_id ~data:queue
    in
    let track_ids_by_bar_id =
      Map.set (t.track_ids_by_bar_id : _ Bar_id.Map.t) ~key:bar_id ~data:track_id
    in
    let track_ids_by_bar_id =
      match remove_id with
      | None -> track_ids_by_bar_id
      | Some id -> Map.remove (track_ids_by_bar_id : _ Bar_id.Map.t) id
    in
    { tracks; track_ids_by_bar_id; last_trigger_time = now }
  ;;

  let finish_bar t ~now (bar_id : Bar_id.t) : t =
    let updated_tracks =
      let open Option.Let_syntax in
      let%bind track_id = Map.find (t.track_ids_by_bar_id : _ Bar_id.Map.t) bar_id in
      let%bind queue = Map.find (t.tracks : _ Track_id.Map.t) track_id in
      let%map element = Fdeque.peek_front queue in
      let updated_element = Bar.finish element ~now in
      let updated_queue =
        let q = Fdeque.drop_front_exn queue in
        Fdeque.enqueue q `front updated_element
      in
      Map.set (t.tracks : _ Track_id.Map.t) ~key:track_id ~data:updated_queue
    in
    match updated_tracks with
    | Some tracks -> { t with tracks }
    | None -> t
  ;;
end

module Tracks_action = struct
  type t =
    | Add_bar of Bar_id.t
    | Finish_bar of Bar_id.t
  [@@deriving sexp, equal]
end

let time_ns_to_string time =
  Time_ns.to_string_abs_parts time ~zone:Time_float.Zone.utc |> List.last_exn
;;

let time_span_to_pixels span = Int.to_float (Time_ns.Span.to_int_ms span) /. 25.0

let timeline ~now ~(tracks : Bar.t Fdeque.t Track_id.Map.t Value.t) =
  let timeline_width = 200.0 in
  let timeline_height = 100.0 in
  let starting_line =
    Virtual_dom_svg.(
      Node.line
        ~attr:
          (Vdom.Attr.many
             [ Attr.x1 200.
             ; Attr.x2 200.
             ; Attr.y1 (-10.)
             ; Attr.y2 105.
             ; Attr.stroke (`Name "white")
             ; Attr.stroke_width 4.0
             ; Attr.stroke_dasharray [ 5.; 5. ]
             ])
        [])
  in
  let%sub number_of_tracks =
    let%arr tracks = tracks in
    Map.length (tracks : _ Track_id.Map.t)
  in
  let%sub tracks =
    Bonsai.assoc
      (module Track_id)
      tracks
      ~f:(fun track_id deque ->
        let%arr now = now
        and number_of_tracks = number_of_tracks
        and deque = deque
        and track_id = track_id in
        let deque = Fdeque.to_list deque in
        List.map deque ~f:(fun { start_time; end_time; id } ->
          let end_time = Option.value end_time ~default:now in
          let start_time_pixels = time_span_to_pixels (Time_ns.diff now start_time) in
          let x = timeline_width -. start_time_pixels in
          let width = time_span_to_pixels (Time_ns.diff end_time start_time) in
          let height = (timeline_height /. Int.to_float number_of_tracks) -. 5.0 in
          let y = (height +. 5.0) *. Int.to_float (Track_id.to_int track_id) in
          Virtual_dom_svg.(
            Node.rect
              ~attr:
                (Vdom.Attr.many
                   [ Attr.x x
                   ; Attr.y y
                   ; Attr.width width
                   ; Attr.height height
                   ; Attr.rx 3.0
                   ; Attr.fill (`Name (bar_id_to_color id))
                   ])
              [])))
  in
  let%sub lines =
    let%arr now = now in
    let times_where_lines_should_be_on =
      List.init 22 ~f:(fun i ->
        Time_ns.prev_multiple
          ~base:Time_ns.epoch
          ~can_equal_before:false
          ~before:(Time_ns.sub now (Time_ns.Span.of_sec (1.0 *. Int.to_float i)))
          ~interval:(Time_ns.Span.of_sec 1.0)
          ())
    in
    List.map times_where_lines_should_be_on ~f:(fun time ->
      let offset = time_span_to_pixels (Time_ns.diff now time) in
      let x = timeline_width -. offset in
      Virtual_dom_svg.(
        Node.line
          ~attr:
            (Vdom.Attr.many
               [ Attr.x1 x
               ; Attr.x2 x
               ; Attr.y1 (-10.0)
               ; Attr.y2 105.
               ; Attr.stroke (`Name "white")
               ; Attr.stroke_width 3.0
               ; Attr.stroke_dasharray [ 5.; 5. ]
               ])
          []))
  in
  let%arr tracks = tracks
  and lines = lines in
  let bars =
    Virtual_dom_svg.(
      Node.g
        ~attr:(Attr.transform [ Attr.Translate { dx = 0.0; dy = 5.0 } ])
        (List.concat (Map.data (tracks : _ Track_id.Map.t))))
  in
  Virtual_dom_svg.(
    Node.svg
      ~attr:
        (Vdom.Attr.many
           [ Attr.width timeline_width; Attr.height (timeline_height +. 5.0) ])
      ([ bars; starting_line ] @ lines))
;;

let clock
      ~trigger_on_activate
      ~when_to_start_next_effect
      ~title
      ~description
      ~wait_time
      ~delta_time:now
  =
  let%sub next_bar_id = Bar_id.component in
  let%sub { tracks; last_trigger_time; _ }, update_tracks =
    Bonsai.state_machine1
      (module Tracks)
      (module Tracks_action)
      ~default_model:Tracks.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ now tracks action ->
        match action with
        | Add_bar bar_id -> Tracks.add_bar tracks ~now bar_id
        | Finish_bar bar_id -> Tracks.finish_bar tracks ~now bar_id)
      now
  in
  let%sub timeline = timeline ~now ~tracks in
  let%sub clock_action =
    let%arr wait_time = wait_time
    and update_tracks = update_tracks
    and next_bar_id = next_bar_id in
    let open Effect.Let_syntax in
    let%bind bar_id = next_bar_id in
    let%bind wait_time = Effect.of_sync_fun Random_time_span.get_wait_time wait_time in
    let%bind () = update_tracks (Add_bar bar_id) in
    let%bind () = Effect.of_deferred_fun Async_kernel.Clock_ns.after wait_time in
    update_tracks (Finish_bar bar_id)
  in
  let%sub () =
    Bonsai.Clock.every
      ~trigger_on_activate
      ~when_to_start_next_effect
      (Time_ns.Span.of_sec 1.0)
      clock_action
  in
  let%arr last_trigger_time = last_trigger_time
  and timeline = timeline in
  Vdom.Node.div
    ~attr:(Vdom.Attr.many [ paper; column ])
    [ Vdom.Node.strong [ Vdom.Node.text title ]
    ; Vdom.Node.div ~attr:row [ Vdom.Node.text description ]
    ; Vdom.Node.div
        [ Vdom.Node.text ("Last triggered: " ^ time_ns_to_string last_trigger_time) ]
    ; Vdom.Node.div ~attr:(Vdom.Attr.many [ timeline_frame; row ]) [ timeline ]
    ]
;;

let all_clocks ~trigger_on_activate ~wait_time ~delta_time =
  List.map
    [ ( `Wait_period_after_previous_effect_finishes_blocking
      , "`Wait_period_after_previous_effect_finishes_blocking"
      , "Always waits clock span after action was finished." )
    ; ( `Wait_period_after_previous_effect_starts_blocking
      , "`Wait_period_after_previous_effect_starts_blocking"
      , "If action takes longer than span, then clock will trigger immediately after \
         current action is done." )
    ; ( `Every_multiple_of_period_blocking
      , "`Every_multiple_of_period_blocking"
      , "Keeps its rythmn even on missed beats, will always trigger on multiples of the \
         span." )
    ; ( `Every_multiple_of_period_non_blocking
      , "`Every_multiple_of_period_non_blocking"
      , "Does not wait for previous effect to finish. Effects can overlap." )
    ]
    ~f:(fun (when_to_start_next_effect, title, description) ->
      clock
        ~trigger_on_activate
        ~when_to_start_next_effect
        ~title
        ~description
        ~wait_time
        ~delta_time)
  |> Computation.all
;;

let immediate_clocks ~wait_time = all_clocks ~trigger_on_activate:true ~wait_time

let component =
  let%sub time_span_form = Random_time_span.form in
  let%sub wait_time =
    let%arr time_span_form = time_span_form in
    Form.value_or_default time_span_form ~default:Random_time_span.default
  in
  let%sub now = Bonsai.Clock.now in
  let%sub initial_time = Bonsai.freeze (module Time_ns.Alternate_sexp) now in
  let%sub delta_time =
    let%arr initial_time = initial_time
    and now = now in
    Time_ns.sub now (Time_ns.to_span_since_epoch initial_time)
  in
  let%sub immediate_clocks = immediate_clocks ~wait_time ~delta_time in
  let%arr immediate_clocks = immediate_clocks
  and wait_time_form = time_span_form
  and wait_time = wait_time in
  Vdom.Node.div
    ~attr:column
    [ Vdom.Node.div
        ~attr:column
        [ Vdom.Node.div ~attr:row [ Vdom.Node.text "Clock ticks every 1s" ]
        ; Vdom.Node.div
            ~attr:(Vdom.Attr.many [ paper; row ])
            [ Form.view_as_vdom wait_time_form ]
        ; Vdom.Node.div
            ~attr:row
            [ Vdom.Node.text
                ("Current duration: "
                 ^ Sexp.to_string (Random_time_span.sexp_of_t wait_time)
                 ^ "s + some small (but non-zero) overhead.")
            ]
        ]
    ; Vdom.Node.div ~attr:(Vdom.Attr.many [ paper; row ]) immediate_clocks
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
