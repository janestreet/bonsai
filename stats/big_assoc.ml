open! Core
open! Bonsai
open! Bonsai_test
open Bonsai.Let_syntax

let () =
  let big_map =
    Bonsai.Value.return (List.init 100 ~f:(fun i -> i, i) |> Int.Map.of_alist_exn)
  in
  let component =
    Bonsai.assoc
      (module Int)
      big_map
      ~f:(fun key data ->
        let%sub my_state = Bonsai.state_opt [%here] (module String) in
        let%sub something =
          return
            (let%map _my_state = my_state
             and _key = key in
             ())
        in
        return
          (let%map _something = something
           and _data = data in
           ()))
  in
  let handle = Handle.create Result_spec.invisible component in
  Handle.recompute_view handle;
  let result_incr = Handle.result_incr handle in
  Ui_incr.Packed.save_dot Out_channel.stdout [ Ui_incr.pack result_incr ]
;;
