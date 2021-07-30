open Core
open Js_of_ocaml
open Bonsai.Let_syntax

module Svar = Bonsai.Effect.For_testing.Svar

module Progress = struct
  type t =
    { loaded : int
    ; total : int
    }
  [@@deriving compare, equal, sexp]

  let to_percentage { loaded; total } = Percent.of_mult (float loaded /. float total)
end

module File_read_state = struct
  type t =
    | Contents of string
    | Loading of Progress.t option
    | Error of Error.t
  [@@deriving compare, equal, sexp]
end

module Test_data = struct
  module State = struct
    type t =
      | Open of
          { chunks : string Reversed_list.t
          ; total_bytes : int
          }
      | Closed of string Or_error.t

    let to_file_read_state = function
      | Closed (Error e) -> File_read_state.Error e
      | Closed (Ok str) -> Contents str
      | Open { chunks; total_bytes } ->
        let loaded =
          (* Have to rev the list here as Reversed_list does not support any real operations
             (e.g. fold) *)
          List.fold (Reversed_list.rev chunks) ~init:0 ~f:(fun acc chunk ->
            acc + String.length chunk)
        in
        Loading (Some { loaded; total = total_bytes })
    ;;
  end

  type t =
    { filename : string
    ; state_var : State.t Bonsai.Var.t
    ; state_bus : (File_read_state.t -> unit) Bus.Read_write.t
    ; result : string Or_error.t Svar.t
    }

  let on_state_change t here ~f =
    ignore (Bus.subscribe_exn (Bus.read_only t.state_bus) here ~f : _ Bus.Subscriber.t)
  ;;

  let create_state_bus here =
    Bus.create
      here
      Arity1
      ~on_subscription_after_first_write:Allow_and_send_last_value
      ~on_callback_raise:Error.raise
  ;;

  let create_stream ~filename ~total_bytes =
    { filename
    ; state_var = Bonsai.Var.create (State.Open { chunks = []; total_bytes })
    ; state_bus = create_state_bus [%here]
    ; result = Svar.create ()
    }
  ;;

  let create_static ~filename ~contents =
    let result = Svar.create () in
    (* No re-entrancy concerns: there can be no handlers for [result] as we just created
       it. *)
    Svar.fill_if_empty result (Ok contents);
    { filename
    ; state_var = Bonsai.Var.create (State.Closed (Ok contents))
    ; state_bus = create_state_bus [%here]
    ; result
    }
  ;;

  let state t = Bonsai.Var.value t.state_var >>| State.to_file_read_state

  let update_state t ~f =
    Bonsai.Var.update t.state_var ~f;
    Bus.write t.state_bus (Bonsai.Var.get t.state_var |> State.to_file_read_state)
  ;;

  let feed_exn t chunk =
    update_state t ~f:(function
      | Closed _ ->
        raise_s [%message "Bonsai_web_ui_file.Test_data.feed: already closed"]
      | Open { chunks; total_bytes } -> Open { chunks = chunk :: chunks; total_bytes })
  ;;

  let close t =
    match Bonsai.Var.get t.state_var with
    | Closed _ -> ()
    | Open { chunks; total_bytes = _ } ->
      let result = Ok (Reversed_list.rev chunks |> String.concat ~sep:"") in
      update_state t ~f:(const (State.Closed result));
      (* No re-entrancy concerns, as this is the last thing we do *)
      Svar.fill_if_empty t.result result
  ;;

  let close_error t error =
    match Bonsai.Var.get t.state_var with
    | Closed _ -> ()
    | Open _ ->
      let result = Error error in
      update_state t ~f:(const (State.Closed result));
      (* No re-entrancy concerns, as this is the last thing we do *)
      Svar.fill_if_empty t.result result
  ;;
end

type t =
  | In_browser of File.file Js.t
  | In_tests of Test_data.t

let filename = function
  | In_browser file -> File.filename file |> Js.to_string
  | In_tests test_data -> test_data.filename
;;

let sexp_of_t t = Sexp.Atom [%string "<file %{filename t#String}>"]

module File_read = struct
  module Progress = Progress
  module State = File_read_state

  type error =
    | Aborted
    | Error of Error.t
  [@@deriving compare, sexp_of]

  module In_browser = struct
    type t =
      { state : State.t Bonsai.Var.t
      ; on_state_change : State.t -> unit Ui_effect.t
      ; file_reader : File.fileReader Js.t
      ; result : (string, error) Result.t Svar.t
      }

    let update_state t ~f =
      Bonsai.Var.update t.state ~f;
      t.on_state_change (Bonsai.Var.get t.state) |> Ui_effect.Expert.handle
    ;;

    let update_progress t ev =
      let progress : Progress.t option =
        if Js.to_bool ev##.lengthComputable
        then Some { loaded = ev##.loaded; total = ev##.total }
        else None
      in
      update_state t ~f:(function
        | (Contents _ | Error _) as x ->
          (* Don't update if we have already reached one of these terminal states *)
          x
        | Loading _ -> Loading progress)
    ;;

    let state t = Bonsai.Var.value t.state

    let abort t =
      t.file_reader##abort;
      (* No re-entrancy concerns as this is the last thing we do. *)
      Svar.fill_if_empty t.result (Error Aborted)
    ;;

    let create file ~on_state_change =
      let file_reader = new%js File.fileReader in
      let t =
        { state = Bonsai.Var.create (File_read_state.Loading None)
        ; on_state_change
        ; result = Svar.create ()
        ; file_reader
        }
      in
      file_reader##.onprogress
      := Dom.handler (fun ev ->
        update_progress t ev;
        Js._true);
      file_reader##.onerror
      := Dom.handler (fun _ev ->
        let error =
          Error.create_s
            [%message
              "Error reading file" ~code:(file_reader##.error##.code : int)]
        in
        update_state t ~f:(const (File_read_state.Error error));
        (* No re-entrancy concerns: handlers attached to [t.result] cannot re-invoke this
           callback. *)
        Svar.fill_if_empty t.result (Error (Error error));
        Js._true);
      file_reader##.onload
      := Dom.handler (fun ev ->
        update_progress t ev;
        (match
           t.file_reader##.result |> File.CoerceTo.arrayBuffer |> Js.Opt.to_option
         with
         | None ->
           raise_s
             [%message "BUG: could not coerce fileReader result to arrayBuffer"]
         | Some array_buffer ->
           let contents = Typed_array.String.of_arrayBuffer array_buffer in
           update_state t ~f:(const (File_read_state.Contents contents));
           (* No re-entrancy concerns: handlers attached to [t.result] cannot re-invoke this
              callback. *)
           Svar.fill_if_empty t.result (Ok contents));
        Js._true);
      t.file_reader##readAsArrayBuffer file;
      t
    ;;
  end

  type t =
    | In_tests of
        { test_data : Test_data.t
        ; result : (string, error) Result.t Svar.t
        }
    | In_browser of In_browser.t

  let create_in_browser file ~on_state_change =
    In_browser (In_browser.create file ~on_state_change)
  ;;

  let create_in_tests (test_data : Test_data.t) ~on_state_change =
    Test_data.on_state_change test_data [%here] ~f:(fun state ->
      on_state_change state |> Ui_effect.Expert.handle);
    let result = Svar.create () in
    Svar.upon test_data.result (function
      | Ok _ as ok -> Svar.fill_if_empty result ok
      | Error e -> Svar.fill_if_empty result (Error (Error e)));
    In_tests { test_data; result }
  ;;

  let abort = function
    | In_browser in_browser -> In_browser.abort in_browser
    | In_tests { result; _ } -> Svar.fill_if_empty result (Error Aborted)
  ;;

  let state = function
    | In_browser in_browser -> In_browser.state in_browser
    | In_tests { test_data; _ } -> Test_data.state test_data
  ;;

  let result_svar = function
    | In_browser in_browser -> in_browser.result
    | In_tests { result; _ } -> result
  ;;

  let result = Bonsai.Effect.For_testing.of_svar_fun (fun (t : t) -> result_svar t)
end

let read ?(on_state_change = Fn.const Ui_effect.Ignore) = function
  | In_browser file -> File_read.create_in_browser file ~on_state_change
  | In_tests test_data -> File_read.create_in_tests test_data ~on_state_change
;;

let contents t =
  let file_read = read t in
  match%map.Bonsai.Effect File_read.result file_read with
  | Error Aborted ->
    raise_s [%message "Got Aborted result from file read, but we did not abort"]
  | Error (Error e) -> Result.Error e
  | Ok contents -> Ok contents
;;

module Expert = struct
  let create file = In_browser file
end

module For_testing = struct
  module Test_data = Test_data

  let create test_data = In_tests test_data
end
