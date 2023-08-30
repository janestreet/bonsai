open! Core
open! Import

let baseline
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let reset ~inject_dynamic ~inject_static ~schedule_event (model_from, model_into) =
    let model_from =
      let inject_static action = inject_static (First action) in
      let inject_dynamic action = inject_dynamic (First action) in
      info_from.reset ~inject_dynamic ~inject_static ~schedule_event model_from
    in
    let model_into =
      let inject_static action = inject_static (Second action) in
      let inject_dynamic action = inject_dynamic (Second action) in
      info_into.reset ~inject_dynamic ~inject_static ~schedule_event model_into
    in
    model_from, model_into
  in
  let apply_static ~inject_dynamic ~inject_static ~schedule_event (model_from, model_into)
    = function
    | First action ->
      let inject_static action = inject_static (First action) in
      let inject_dynamic action = inject_dynamic (First action) in
      let model_from =
        info_from.apply_static
          ~inject_dynamic
          ~inject_static
          ~schedule_event
          model_from
          action
      in
      model_from, model_into
    | Second action ->
      let inject_static action = inject_static (Second action) in
      let inject_dynamic action = inject_dynamic (Second action) in
      let model_into =
        info_into.apply_static
          ~inject_dynamic
          ~inject_static
          ~schedule_event
          model_into
          action
      in
      model_from, model_into
  in
  let apply_dynamic
    ~inject_dynamic
    ~inject_static
    ~schedule_event
    input
    (model_from, model_into)
    = function
    | First action ->
      let inject_static action = inject_static (First action) in
      let inject_dynamic action = inject_dynamic (First action) in
      let model_from =
        info_from.apply_dynamic
          ~inject_dynamic
          ~inject_static
          ~schedule_event
          (Option.map input ~f:fst)
          model_from
          action
      in
      model_from, model_into
    | Second action ->
      let inject_static action = inject_static (Second action) in
      let inject_dynamic action = inject_dynamic (Second action) in
      let model_into =
        info_into.apply_dynamic
          ~inject_dynamic
          ~inject_static
          ~schedule_event
          (Option.map input ~f:snd)
          model_into
          action
      in
      model_from, model_into
  in
  let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
    let from =
      let inject_dynamic effect = inject_dynamic (First effect) in
      let inject_static effect = inject_static (First effect) in
      let model = Incr.map model ~f:Tuple2.get1 in
      let path = Path.append path Path.Elem.Subst_from in
      info_from.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let inject_dynamic effect = inject_dynamic (Second effect) in
      let inject_static effect = inject_static (Second effect) in
      let model = Incr.map model ~f:Tuple2.get2 in
      let path = Path.append path Path.Elem.Subst_into in
      info_into.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Input.merge (Snapshot.input from) (Snapshot.input into) in
    Snapshot.create ~result ~input ~lifecycle
  in
  let model = Meta.Model.both info_from.model info_into.model in
  let dynamic_action =
    Meta.Action.both info_from.dynamic_action info_into.dynamic_action
  in
  let static_action = Meta.Action.both info_from.static_action info_into.static_action in
  let input = Meta.Input.both info_from.input info_into.input in
  Computation.T
    { model
    ; input
    ; dynamic_action
    ; static_action
    ; apply_static
    ; apply_dynamic
    ; run
    ; reset
    }
;;

let from_stateless
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
    let from =
      let path = Path.append path Path.Elem.Subst_from in
      info_from.run
        ~environment
        ~path
        ~clock
        ~model:unit_model
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static:Nothing.unreachable_code
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let path = Path.append path Path.Elem.Subst_into in
      info_into.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Snapshot.input into in
    Snapshot.create ~result ~input ~lifecycle
  in
  Computation.T
    { run
    ; input = info_into.input
    ; model = info_into.model
    ; dynamic_action = info_into.dynamic_action
    ; static_action = info_into.static_action
    ; apply_static = info_into.apply_static
    ; apply_dynamic = info_into.apply_dynamic
    ; reset = info_into.reset
    }
;;

let into_stateless
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static =
    let from =
      let path = Path.append path Path.Elem.Subst_from in
      info_from.run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let path = Path.append path Path.Elem.Subst_into in
      info_into.run
        ~environment
        ~path
        ~clock
        ~model:unit_model
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static:Nothing.unreachable_code
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Snapshot.input from in
    Snapshot.create ~result ~input ~lifecycle
  in
  Computation.T
    { run
    ; input = info_from.input
    ; model = info_from.model
    ; dynamic_action = info_from.dynamic_action
    ; static_action = info_from.static_action
    ; apply_static = info_from.apply_static
    ; apply_dynamic = info_from.apply_dynamic
    ; reset = info_from.reset
    }
;;

let no_static_actions
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let reset ~inject_dynamic:_ ~inject_static ~schedule_event (model_from, model_into) =
    let model_from =
      let inject_static action = inject_static (First action) in
      info_from.reset
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static
        ~schedule_event
        model_from
    in
    let model_into =
      let inject_static action = inject_static (Second action) in
      info_into.reset
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static
        ~schedule_event
        model_into
    in
    model_from, model_into
  in
  let apply_static
    ~inject_dynamic:_
    ~inject_static
    ~schedule_event
    (model_from, model_into)
    = function
    | First action ->
      let inject_static action = inject_static (First action) in
      let model_from =
        info_from.apply_static
          ~inject_dynamic:Nothing.unreachable_code
          ~inject_static
          ~schedule_event
          model_from
          action
      in
      model_from, model_into
    | Second action ->
      let inject_static action = inject_static (Second action) in
      let model_into =
        info_into.apply_static
          ~inject_dynamic:Nothing.unreachable_code
          ~inject_static
          ~schedule_event
          model_into
          action
      in
      model_from, model_into
  in
  let apply_dynamic ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ _ _ =
    Nothing.unreachable_code
  in
  let run ~environment ~path ~clock ~model ~inject_dynamic:_ ~inject_static =
    let from =
      let inject_static effect = inject_static (First effect) in
      let model = Incr.map model ~f:Tuple2.get1 in
      let path = Path.append path Path.Elem.Subst_from in
      info_from.run
        ~environment
        ~path
        ~clock
        ~model
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let inject_static effect = inject_static (Second effect) in
      let model = Incr.map model ~f:Tuple2.get2 in
      let path = Path.append path Path.Elem.Subst_into in
      info_into.run
        ~environment
        ~path
        ~clock
        ~model
        ~inject_dynamic:Nothing.unreachable_code
        ~inject_static
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Input.merge (Snapshot.input from) (Snapshot.input into) in
    Snapshot.create ~result ~input ~lifecycle
  in
  let model = Meta.Model.both info_from.model info_into.model in
  let static_action = Meta.Action.both info_from.static_action info_into.static_action in
  let input = Meta.Input.both info_from.input info_into.input in
  Computation.T
    { model
    ; input
    ; dynamic_action = info_from.dynamic_action
    ; static_action
    ; apply_static
    ; apply_dynamic
    ; run
    ; reset
    }
;;

let no_dynamic_actions
  ~here
  ~(info_from : _ Computation.info)
  ~(info_into : _ Computation.info)
  ~via
  =
  let reset ~inject_dynamic ~inject_static:_ ~schedule_event (model_from, model_into) =
    let model_from =
      let inject_dynamic action = inject_dynamic (First action) in
      info_from.reset
        ~inject_dynamic
        ~inject_static:Nothing.unreachable_code
        ~schedule_event
        model_from
    in
    let model_into =
      let inject_dynamic action = inject_dynamic (Second action) in
      info_into.reset
        ~inject_dynamic
        ~inject_static:Nothing.unreachable_code
        ~schedule_event
        model_into
    in
    model_from, model_into
  in
  let apply_static ~inject_dynamic:_ ~inject_static:_ ~schedule_event:_ _ =
    Nothing.unreachable_code
  in
  let apply_dynamic
    ~inject_dynamic
    ~inject_static:_
    ~schedule_event
    input
    (model_from, model_into)
    = function
    | First action ->
      let inject_dynamic action = inject_dynamic (First action) in
      let model_from =
        info_from.apply_dynamic
          ~inject_dynamic
          ~inject_static:Nothing.unreachable_code
          ~schedule_event
          (Option.map input ~f:fst)
          model_from
          action
      in
      model_from, model_into
    | Second action ->
      let inject_dynamic action = inject_dynamic (Second action) in
      let model_into =
        info_into.apply_dynamic
          ~inject_dynamic
          ~inject_static:Nothing.unreachable_code
          ~schedule_event
          (Option.map input ~f:snd)
          model_into
          action
      in
      model_from, model_into
  in
  let run ~environment ~path ~clock ~model ~inject_dynamic ~inject_static:_ =
    let from =
      let inject_dynamic effect = inject_dynamic (First effect) in
      let model = Incr.map model ~f:Tuple2.get1 in
      let path = Path.append path Path.Elem.Subst_from in
      info_from.run
        ~environment
        ~path
        ~clock
        ~model
        ~inject_dynamic
        ~inject_static:Nothing.unreachable_code
    in
    Snapshot.attribute_positions here from;
    let from_result = Snapshot.result from in
    let environment = Environment.add_exn environment ~key:via ~data:from_result in
    let into =
      let inject_dynamic effect = inject_dynamic (Second effect) in
      let model = Incr.map model ~f:Tuple2.get2 in
      let path = Path.append path Path.Elem.Subst_into in
      info_into.run
        ~environment
        ~path
        ~clock
        ~model
        ~inject_dynamic
        ~inject_static:Nothing.unreachable_code
    in
    let result = Snapshot.result into in
    let lifecycle =
      Option.merge
        (Snapshot.lifecycle from)
        (Snapshot.lifecycle into)
        ~f:Lifecycle.Collection.merge
    in
    let input = Input.merge (Snapshot.input from) (Snapshot.input into) in
    Snapshot.create ~result ~input ~lifecycle
  in
  let model = Meta.Model.both info_from.model info_into.model in
  let dynamic_action =
    Meta.Action.both info_from.dynamic_action info_into.dynamic_action
  in
  let input = Meta.Input.both info_from.input info_into.input in
  Computation.T
    { model
    ; input
    ; dynamic_action
    ; static_action = info_from.static_action
    ; apply_static
    ; apply_dynamic
    ; run
    ; reset
    }
;;
