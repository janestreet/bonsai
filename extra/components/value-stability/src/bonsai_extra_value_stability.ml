open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let with_last_modified_time
  ~equal
  input
  (* Although [Bonsai.Clock.Expert.now] is generally discouraged, the cutoff only pays
     attention to [input], so [now] shouldn't cause re-firing of this computation's
     transitive dependencies. *)
  (local_ graph)
  =
  let now = Bonsai.Clock.Expert.now graph in
  let result = Bonsai.both input now in
  let%sub result, time =
    Bonsai.Incr.value_cutoff result ~equal:(fun (a, _) (b, _) -> equal a b) graph
  in
  result, time
;;

let is_stable ~equal input ~time_to_stable (local_ graph) =
  let sign =
    let%arr time_to_stable in
    Time_ns.Span.sign time_to_stable
  in
  match%sub sign with
  | Neg ->
    let on_activate =
      Bonsai.return
        (Effect.of_thunk (fun () ->
           eprint_s
             [%message "Bonsai_extra.is_stable: [time_to_stable] should not be negative"]))
    in
    let () = Bonsai.Edge.lifecycle ~on_activate graph in
    Bonsai.return true
  | Zero -> Bonsai.return true
  | Pos ->
    let _, last_modified_time = with_last_modified_time ~equal input graph in
    let next_stable_time =
      let%arr last_modified_time and time_to_stable in
      Time_ns.add last_modified_time time_to_stable
    in
    let at_next_stable_time = Bonsai.Clock.at next_stable_time graph in
    (match%arr at_next_stable_time with
     | Before -> false
     | After -> true)
;;

let most_recent_value_satisfying ?sexp_of_model ~equal input ~condition =
  Bonsai.most_recent_some ?sexp_of_model ~equal input ~f:(fun a ->
    if condition a then Some a else None)
;;

module Stability = struct
  type 'a t =
    | Stable of 'a
    | Unstable of
        { previously_stable : 'a option
        ; unstable_value : 'a
        }
  [@@deriving sexp, equal]

  let most_recent_stable_value = function
    | Stable a -> Some a
    | Unstable { previously_stable; _ } -> previously_stable
  ;;

  let prefer_stable_value = function
    | Stable a -> a
    | Unstable { previously_stable; unstable_value } ->
      (match previously_stable with
       | Some a -> a
       | None -> unstable_value)
  ;;
end

let value_stability
  (type a)
  ?sexp_of_model
  ~equal:input_equal
  input
  ~time_to_stable
  (local_ graph)
  =
  let module M = struct
    type t = a

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_model
  end
  in
  let is_stable = is_stable ~equal:input_equal input ~time_to_stable graph in
  let most_recent_stable_and_true =
    let input_and_stability = Bonsai.both input is_stable in
    let module M = struct
      include M

      let equal = input_equal
    end
    in
    most_recent_value_satisfying
      ~sexp_of_model:[%sexp_of: M.t * bool]
      ~equal:[%equal: M.t * bool]
      input_and_stability
      ~condition:(fun (_input, is_stable) -> is_stable)
      graph
  in
  match%sub most_recent_stable_and_true with
  | Some most_recent_stable_and_true ->
    let%arr most_recent_stable, must_be_true = most_recent_stable_and_true
    and is_stable
    and input in
    (match must_be_true with
     | true -> ()
     | false ->
       eprint_s [%message "BUG:" [%here] "value which passed through filter must be true"]);
    if input_equal input most_recent_stable && is_stable
    then Stability.Stable input
    else Unstable { previously_stable = Some most_recent_stable; unstable_value = input }
  | None ->
    let%arr input in
    Stability.Unstable { previously_stable = None; unstable_value = input }
;;
