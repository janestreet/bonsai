open Core
open Async_kernel
open Js_of_ocaml

let create file =
  let read =
    Ui_effect.of_sync_fun (fun on_progress ->
      let file_reader = new%js File.fileReader in
      let result = Ivar.create () in
      let result =
        Bonsai_web.Effect.of_deferred_fun
          (fun () ->
             let call_on_progress ev =
               if Js.to_bool ev##.lengthComputable
               then
                 on_progress
                   { Bonsai_web_ui_file.Progress.loaded = ev##.loaded; total = ev##.total }
                 |> Ui_effect.Expert.handle
             in
             file_reader##.onprogress
             := Dom.handler (fun ev ->
               call_on_progress ev;
               Js._true);
             file_reader##.onerror
             := Dom.handler (fun _ev ->
               let error =
                 Error.create_s
                   [%message
                     "Error reading file"
                       ~code:(file_reader##.error##.code : int)
                       ~message:
                         (Js.to_string
                            (Js.Unsafe.get file_reader##.error (Js.string "message")))]
               in
               Ivar.fill_if_empty
                 result
                 (Error (Bonsai_web_ui_file.Read_error.Error error));
               Js._true);
             file_reader##.onload
             := Dom.handler (fun ev ->
               call_on_progress ev;
               (match
                  file_reader##.result
                  |> File.CoerceTo.arrayBuffer
                  |> Js.Opt.to_option
                with
                | None ->
                  raise_s
                    [%message
                      "BUG: could not coerce fileReader result to arrayBuffer"]
                | Some array_buffer ->
                  let contents = Typed_array.String.of_arrayBuffer array_buffer in
                  Ivar.fill_if_empty result (Ok contents));
               Js._true);
             file_reader##readAsArrayBuffer file;
             Ivar.read result)
          ()
      in
      let abort = Ui_effect.of_sync_fun (fun () -> file_reader##abort) () in
      { Bonsai_web_ui_file.Expert.result; abort })
  in
  Bonsai_web_ui_file.Expert.create ~read ~filename:(File.filename file |> Js.to_string)
;;
