open! Core
open! Import

let ( >>> ) f inject b = inject (f b)
let wrap_sub_from inject = Action.sub_from >>> inject
let wrap_sub_into inject = Action.sub_into >>> inject

let baseline
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let reset ~inject ~schedule_event (model_from, model_into) =
    let model_from =
      info_from.reset ~inject:(wrap_sub_from inject) ~schedule_event model_from
    in
    let model_into =
      info_into.reset ~inject:(wrap_sub_into inject) ~schedule_event model_into
    in
    model_from, model_into
  in
  let apply_action ~inject ~schedule_event input (model_from, model_into) = function
    | Action.Sub_from action ->
      let model_from =
        info_from.apply_action
          ~inject:(wrap_sub_from inject)
          ~schedule_event
          (Option.map input ~f:fst)
          model_from
          action
      in
      model_from, model_into
    | Sub_into action ->
      let model_into =
        info_into.apply_action
          ~inject:(wrap_sub_into inject)
          ~schedule_event
          (Option.map input ~f:snd)
          model_into
          action
      in
      model_from, model_into
  in
  let both_use_path = info_from.can_contain_path && info_into.can_contain_path in
  let run ~environment ~path ~clock ~model ~inject =
    let%bind.Trampoline from =
      let model = Incr.map model ~f:Tuple2.get1 in
      let path = if both_use_path then Path.append path Path.Elem.Subst_from else path in
      info_from.run ~environment ~path ~clock ~model ~inject:(wrap_sub_from inject)
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let%bind.Trampoline into =
      let model = Incr.map model ~f:Tuple2.get2 in
      let path = if both_use_path then Path.append path Path.Elem.Subst_into else path in
      info_into.run ~environment ~path ~clock ~model ~inject:(wrap_sub_into inject)
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Input.merge (Snapshot.input from) (Snapshot.input into) in
    Trampoline.return (Snapshot.create ~result ~input ~lifecycle)
  in
  let model = Meta.Model.both info_from.model info_into.model in
  let input = Meta.Input.both info_from.input info_into.input in
  Computation.T
    { model
    ; input
    ; action = Action.Type_id.sub ~from:info_from.action ~into:info_into.action
    ; apply_action
    ; run
    ; reset
    ; can_contain_path = info_from.can_contain_path || info_into.can_contain_path
    }
;;

let from_stateless
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let both_use_path = info_from.can_contain_path && info_into.can_contain_path in
  let run ~environment ~path ~clock ~model ~inject =
    let%bind.Trampoline from =
      let path = if both_use_path then Path.append path Path.Elem.Subst_from else path in
      info_from.run ~environment ~path ~clock ~model:unit_model ~inject:unreachable_action
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let%bind.Trampoline into =
      let path = if both_use_path then Path.append path Path.Elem.Subst_into else path in
      info_into.run ~environment ~path ~clock ~model ~inject
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Snapshot.input into in
    Trampoline.return (Snapshot.create ~result ~input ~lifecycle)
  in
  Computation.T
    { run
    ; input = info_into.input
    ; model = info_into.model
    ; action = info_into.action
    ; apply_action = info_into.apply_action
    ; reset = info_into.reset
    ; can_contain_path = info_from.can_contain_path || info_into.can_contain_path
    }
;;

let into_stateless
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let both_use_path = info_from.can_contain_path && info_into.can_contain_path in
  let run ~environment ~path ~clock ~model ~inject =
    let%bind.Trampoline from =
      let path = if both_use_path then Path.append path Path.Elem.Subst_from else path in
      info_from.run ~environment ~path ~clock ~model ~inject
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let%bind.Trampoline into =
      let path = if both_use_path then Path.append path Path.Elem.Subst_into else path in
      info_into.run ~environment ~path ~clock ~model:unit_model ~inject:unreachable_action
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Snapshot.input from in
    Trampoline.return (Snapshot.create ~result ~input ~lifecycle)
  in
  Computation.T
    { run
    ; input = info_from.input
    ; model = info_from.model
    ; action = info_from.action
    ; apply_action = info_from.apply_action
    ; reset = info_from.reset
    ; can_contain_path = info_from.can_contain_path || info_into.can_contain_path
    }
;;
