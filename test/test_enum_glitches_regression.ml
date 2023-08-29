open! Core
open! Import
module Bonsai_lib = Bonsai
open Proc

module Choice = struct
  type t =
    | Homepage
    | Loading
    | Search_results
  [@@deriving sexp, equal]
end

let apply_action context _ _ new_page =
  (match new_page with
   | Choice.Homepage | Search_results -> ()
   | Loading ->
     Bonsai.Apply_action_context.schedule_event
       context
       (Bonsai.Apply_action_context.inject context Choice.Search_results));
  new_page
;;

module Result = struct
  type t =
    { view : string
    ; incoming : Choice.t -> unit Ui_effect.t
    }
  [@@deriving fields ~getters]

  type incoming = Choice.t
end

let%expect_test _ =
  let open Bonsai.Let_syntax in
  let graph =
    let%sub state_machine =
      Bonsai.state_machine1
        ~sexp_of_model:[%sexp_of: Choice.t]
        ~equal:[%equal: Choice.t]
        ~sexp_of_action:[%sexp_of: Choice.t]
        ~default_model:Choice.Homepage
        ~apply_action
        (Bonsai_lib.Value.return ())
    in
    let%sub current_page =
      Bonsai.read
        (let%map current_page, _ = state_machine in
         current_page)
    in
    let%sub incoming =
      Bonsai.read
        (let%map _, incoming = state_machine in
         incoming)
    in
    let as_eithers =
      match%map current_page with
      | Loading -> First (Second ())
      | Homepage -> First (Second ())
      | Search_results -> Second ()
    in
    (* Before, this test was preventing a bug from popping up again. However,
       this test depended on [match_either], which has now been deleted. Thus,
       we can't know whether this test would catch any regressions of the bug,
       so it's just an ordinary test now *)
    let%sub body =
      match%sub as_eithers with
      | First (First _) -> Bonsai.const "1"
      | First (Second _) -> Bonsai.const "2"
      | Second _ -> Bonsai.const "3"
    in
    Bonsai.read
      (let%map view = body
       and incoming = incoming in
       { Result.view; incoming })
  in
  let handle = Handle.create (module Result) graph in
  Handle.show handle;
  [%expect {| 2 |}];
  Handle.do_actions handle [ Search_results ];
  Handle.show handle;
  [%expect {| 3 |}]
;;
