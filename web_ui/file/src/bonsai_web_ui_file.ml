open Core
open Bonsai.Let_syntax

module Progress = struct
  type t =
    { loaded : int
    ; total : int
    }
  [@@deriving compare, equal, sexp]

  let to_percentage { loaded; total } = Percent.of_mult (float loaded /. float total)
end

module _ = struct
  type t =
    | Contents of string
    | Loading of Progress.t option
    | Error of Error.t
  [@@deriving compare, equal, sexp]
end

module Read_error = struct
  type t =
    | Aborted
    | Error of Error.t
  [@@deriving compare, equal, sexp]
end

module File_read = struct
  type t =
    { result : (string, Read_error.t) Result.t Ui_effect.t
    ; abort : unit Ui_effect.t
    }
  [@@deriving fields]
end

type t =
  { read : (Progress.t -> unit Ui_effect.t) -> File_read.t Ui_effect.t
  ; filename : string
  }
[@@deriving fields]

let sexp_of_t t = Sexp.Atom [%string "<file %{filename t#String}>"]
let read ?(on_progress = fun _progress -> Ui_effect.Ignore) t = t.read on_progress

let contents t =
  let open Ui_effect.Let_syntax in
  match%map read t >>= File_read.result with
  | Ok contents -> Ok contents
  | Error Aborted -> assert false
  | Error (Error e) -> Error e
;;

module Expert = struct
  type file_read = File_read.t =
    { result : (string, Read_error.t) Result.t Ui_effect.t
    ; abort : unit Ui_effect.t
    }

  let create = Fields.create
end

module For_testing = struct
  module Test_data = struct
    type data =
      | Closed of string Or_error.t
      | Open of
          { chunks : string Queue.t
          ; total_bytes : int
          }

    type read_callbacks =
      { on_progress : Progress.t -> unit
      ; on_finished : string Or_error.t -> unit
      }

    module Read_state = struct
      type t =
        | Not_reading
        | Aborted
        | Reading of read_callbacks

      let iter t ~f =
        match t with
        | Not_reading | Aborted -> ()
        | Reading callbacks -> f callbacks
      ;;
    end

    type t =
      { filename : string
      ; mutable data : data
      ; mutable read_state : Read_state.t
      }

    let create_stream ~filename ~total_bytes =
      { filename
      ; data = Open { chunks = Queue.create (); total_bytes }
      ; read_state = Not_reading
      }
    ;;

    let create_static ~filename ~contents =
      { filename; data = Closed (Ok contents); read_state = Not_reading }
    ;;

    let read_status t =
      match t.read_state with
      | Not_reading -> `Not_reading
      | Aborted -> `Aborted
      | Reading _ -> `Reading
    ;;

    let read t read =
      (match t.data with
       | Open { chunks; total_bytes } ->
         read.on_progress
           { Progress.loaded = Queue.sum (module Int) chunks ~f:String.length
           ; total = total_bytes
           }
       | Closed result -> read.on_finished result);
      t.read_state <- Reading read
    ;;

    let abort_read t = t.read_state <- Aborted

    let feed_exn t chunk =
      match t.data with
      | Open { chunks; total_bytes } ->
        Queue.enqueue chunks chunk;
        let progress =
          { Progress.loaded = Queue.sum (module Int) chunks ~f:String.length
          ; total = total_bytes
          }
        in
        Read_state.iter t.read_state ~f:(fun read -> read.on_progress progress)
      | Closed _ -> raise_s [%message "Bonsai_web_ui_file.Test_data.feed: already closed"]
    ;;

    let close t =
      match t.data with
      | Closed _ -> ()
      | Open { chunks; total_bytes = _ } ->
        let result = Ok (Queue.to_list chunks |> String.concat) in
        t.data <- Closed result;
        Read_state.iter t.read_state ~f:(fun read -> read.on_finished result)
    ;;

    let close_error t error =
      match t.data with
      | Closed _ -> ()
      | Open _ ->
        let result = Error error in
        t.data <- Closed result;
        Read_state.iter t.read_state ~f:(fun read -> read.on_finished result)
    ;;
  end

  let create test_data =
    let module Svar = Ui_effect.For_testing.Svar in
    let read on_progress =
      let (result_var : (string, Read_error.t) Result.t Svar.t) = Svar.create () in
      let result =
        Ui_effect.For_testing.of_svar_fun
          (fun () ->
             Test_data.read
               test_data
               { on_progress =
                   (fun progress -> on_progress progress |> Ui_effect.Expert.handle)
               ; on_finished =
                   (fun result ->
                      Svar.fill_if_empty
                        result_var
                        (Result.map_error result ~f:(fun e -> Read_error.Error e)))
               };
             result_var)
          ()
      in
      let abort =
        Ui_effect.of_sync_fun
          (fun () ->
             Test_data.abort_read test_data;
             Svar.fill_if_empty result_var (Error Aborted))
          ()
      in
      { File_read.result; abort }
    in
    { read = Ui_effect.of_sync_fun read; filename = test_data.filename }
  ;;
end

module Read_on_change = struct
  module File = struct
    type nonrec t = (t[@sexp.opaque]) [@@deriving sexp]

    let equal = phys_equal
  end

  module File_read' = struct
    type t = (File_read.t[@sexp.opaque]) [@@deriving sexp]

    let equal = phys_equal
  end

  module Status = struct
    type t =
      | Starting
      | In_progress of Progress.t
      | Complete of string Or_error.t
    [@@deriving compare, equal, sexp]
  end

  module File_state = struct
    type t =
      | Before_first_read
      | Reading of
          { file_read : File_read'.t
          ; status : Status.t
          }
    [@@deriving equal, sexp]

    let to_status = function
      | Before_first_read -> Status.Starting
      | Reading { status; _ } -> status
    ;;

    module Action = struct
      type t =
        | Start_read of File_read'.t
        | Set_status of Status.t
      [@@deriving equal, sexp]
    end

    let apply_action ~inject:_ ~schedule_event t (action : Action.t) =
      match action with
      | Start_read file_read ->
        (match t with
         | Before_first_read -> ()
         | Reading { file_read = old_file_read; status = _ } ->
           schedule_event (File_read.abort old_file_read));
        Reading { file_read; status = Starting }
      | Set_status status ->
        (match t with
         | Before_first_read -> t
         | Reading { file_read; status = _ } -> Reading { file_read; status })
    ;;

    let abort_read_if_applicable t =
      match%sub t with
      | Before_first_read -> Bonsai.const Ui_effect.Ignore
      | Reading { file_read; status = _ } -> Bonsai.read (file_read >>| File_read.abort)
    ;;
  end

  let create_helper file =
    let%sub state, inject =
      Bonsai.state_machine0
        (module File_state)
        (module File_state.Action)
        ~default_model:Before_first_read
        ~apply_action:File_state.apply_action
    in
    let%sub () =
      let%sub abort = File_state.abort_read_if_applicable state in
      Bonsai.Edge.lifecycle ~on_deactivate:abort ()
    in
    let%sub () =
      Bonsai.Edge.on_change
        (module File)
        file
        ~callback:
          (let%map inject = inject in
           fun file ->
             let open Ui_effect.Let_syntax in
             let%bind file_read =
               read file ~on_progress:(fun progress ->
                 inject (Set_status (In_progress progress)))
             in
             let%bind () = inject (Start_read file_read) in
             match%bind File_read.result file_read with
             | Error Aborted ->
               (* Let the next read take over *)
               return ()
             | Error (Error e) -> inject (Set_status (Complete (Error e)))
             | Ok contents -> inject (Set_status (Complete (Ok contents))))
    in
    Bonsai.read state
  ;;

  let create_multiple files =
    let%sub file_states =
      (* In reality, I suspect that whenever the user changes their selection in a file
         picker widget, the browser generates an entirely new set of File objects for us.
         So I suspect it's not possible for [files] to change in such a way that some, but
         not all, of the keys change. However, it's easy enough to support that, so we do.

         The one thing we don't support is if a file disappears from the map and then
         comes back. In that case, we've already told the file reader to abort the read
         when it disappeared, so there is no way for us to recover. *)
      Bonsai.assoc
        (module Filename)
        files
        ~f:(fun _filename file ->
          let%sub reading = create_helper file in
          Bonsai.read
            (match%map reading with
             | File_state.Before_first_read -> None
             | Reading { status; file_read = _ } -> Some status))
    in
    Bonsai.Incr.compute file_states ~f:(Ui_incr.Map.filter_map ~f:Fn.id)
  ;;

  let create_single file =
    let%sub state = create_helper file in
    let%arr file = file
    and state = state in
    file.filename, File_state.to_status state
  ;;

  let create_single_opt file =
    match%sub file with
    | None -> Bonsai.const None
    | Some file -> Bonsai.Computation.map (create_single file) ~f:Option.some
  ;;
end
