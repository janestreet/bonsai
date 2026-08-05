open! Core
module Bonsai = Bonsai_proc
module Effect = Ui_effect

(* Between each sequenced effect, we keep an open-ended link to the next effect, whenever
   that may occur. Sequencing an effect involves hooking onto the most recent link, and
   then making a new link connected to the newly sequenced effect. Whenever an effect
   finishes, it triggers the link so that the next effect will start. The semantics are
   very similar to those of [Svar].

   If effect completes before the next effect is attached to its link, then we mark the
   link as having already been finished. When the next effect is sees this marking, it
   knows to begin immediately, rather than waiting for the previous effect to complete
   (because it knows that it already has).
*)

type previous =
  { mutable finished : bool
  ; mutable upon_finish : unit -> unit
  }

type t = previous ref

module For_non_bonsai_users = struct
  let create () = ref { finished = true; upon_finish = (fun () -> ()) }
end

let create = Bonsai.Expert.thunk For_non_bonsai_users.create

let run (t : t) ~this_effect_doesn't_call_run:effect_ =
  Effect.Private.make ~request:() ~evaluator:(fun callback ->
    let previous = !t in
    let current = { finished = false; upon_finish = (fun () -> ()) } in
    let on_exn = Effect.Private.Callback.on_exn callback in
    let run_effect () =
      Effect.Expert.eval
        effect_
        ~f:(fun result ->
          Effect.Expert.handle
            (Effect.Private.Callback.respond_to callback result)
            ~on_exn;
          (* It doesn't matter which order these two lines run in. If [upon_finish]
             triggers the next event, then it doesn't matter what [finished] is. If it
             doesn't trigger the next event, then it is just the identity function, which
             won't witness what [finished] is. *)
          current.finished <- true;
          current.upon_finish ())
        ~on_exn
    in
    t := current;
    if previous.finished then run_effect () else previous.upon_finish <- run_effect)
;;
