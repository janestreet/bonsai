open! Core
open! Bonsai
open! Bonsai_web_test

let () =
  let t =
    Bonsai_web_ui_partial_render_table_test.Shared.(
      Test.Component.(
        default
          ()
          (Bonsai.Value.return small_map)
          (Bonsai.Value.return (fun ~key:_ ~data:_ -> true))))
  in
  let handle = Handle.create Result_spec.invisible t.component in
  Handle.recompute_view handle;
  let result_incr = Handle.result_incr handle in
  Ui_incr.Packed.save_dot Out_channel.stdout [ Ui_incr.pack result_incr ]
;;
