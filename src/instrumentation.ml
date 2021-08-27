open! Core
open! Import

let total_label ~current_label label = [%string "%{current_label}-%{label}"]
let id_label ~id label = [%string "%{id#Value_and_computation_id}%{label}"]

let rec instrument_value
  : type a.
    a Value.t
    -> start_timer:(string -> unit)
    -> stop_timer:(string -> unit)
    -> current_label:string
    -> a Value.t
  =
  fun wrapped_value ~start_timer ~stop_timer ~current_label ->
  let value_id = Value_and_computation_id.create () in
  let total_label = total_label ~current_label in
  let id_label = id_label ~id:value_id in
  let instrument_value value ~label =
    instrument_value value ~start_timer ~stop_timer ~current_label:label
  in
  let { Value.value; here } = wrapped_value in
  match value with
  | Constant _ | Incr _ | Named _ -> wrapped_value
  | Both (value_a, value_b) ->
    let label = total_label "both" in
    { Value.value = Both (instrument_value value_a ~label, instrument_value value_b ~label)
    ; here
    }
  | Cutoff { t; equal } ->
    let label = total_label "cutoff" in
    let id_label = id_label label in
    { Value.value =
        Cutoff
          { t = instrument_value t ~label
          ; equal =
              (fun a b ->
                 start_timer id_label;
                 let equal = equal a b in
                 stop_timer id_label;
                 equal)
          }
    ; here
    }
  | Map { t; f } ->
    let label = total_label "map" in
    let id_label = id_label label in
    { Value.value =
        Map
          { t = instrument_value t ~label
          ; f =
              (fun a ->
                 start_timer id_label;
                 let b = f a in
                 stop_timer id_label;
                 b)
          }
    ; here
    }
  | Map2 { t1; t2; f } ->
    let label = total_label "map2" in
    let id_label = id_label label in
    { Value.value =
        Map2
          { t1 = instrument_value t1 ~label
          ; t2 = instrument_value t2 ~label
          ; f =
              (fun a b ->
                 start_timer id_label;
                 let r = f a b in
                 stop_timer id_label;
                 r)
          }
    ; here
    }
  | Map3 { t1; t2; t3; f } ->
    let label = total_label "map3" in
    let id_label = id_label label in
    { Value.value =
        Map3
          { t1 = instrument_value t1 ~label
          ; t2 = instrument_value t2 ~label
          ; t3 = instrument_value t3 ~label
          ; f =
              (fun a b c ->
                 start_timer id_label;
                 let r = f a b c in
                 stop_timer id_label;
                 r)
          }
    ; here
    }
  | Map4 { t1; t2; t3; t4; f } ->
    let label = total_label "map4" in
    let id_label = id_label label in
    { Value.value =
        Map4
          { t1 = instrument_value t1 ~label
          ; t2 = instrument_value t2 ~label
          ; t3 = instrument_value t3 ~label
          ; t4 = instrument_value t4 ~label
          ; f =
              (fun a b c d ->
                 start_timer id_label;
                 let r = f a b c d in
                 stop_timer id_label;
                 r)
          }
    ; here
    }
  | Map5 { t1; t2; t3; t4; t5; f } ->
    let label = total_label "map5" in
    let id_label = id_label label in
    { Value.value =
        Map5
          { t1 = instrument_value t1 ~label
          ; t2 = instrument_value t2 ~label
          ; t3 = instrument_value t3 ~label
          ; t4 = instrument_value t4 ~label
          ; t5 = instrument_value t5 ~label
          ; f =
              (fun a b c d e ->
                 start_timer id_label;
                 let r = f a b c d e in
                 stop_timer id_label;
                 r)
          }
    ; here
    }
  | Map6 { t1; t2; t3; t4; t5; t6; f } ->
    let label = total_label "map6" in
    let id_label = id_label label in
    { Value.value =
        Map6
          { t1 = instrument_value t1 ~label
          ; t2 = instrument_value t2 ~label
          ; t3 = instrument_value t3 ~label
          ; t4 = instrument_value t4 ~label
          ; t5 = instrument_value t5 ~label
          ; t6 = instrument_value t6 ~label
          ; f =
              (fun a b c d e g ->
                 start_timer id_label;
                 let r = f a b c d e g in
                 stop_timer id_label;
                 r)
          }
    ; here
    }
  | Map7 { t1; t2; t3; t4; t5; t6; t7; f } ->
    let label = total_label "map7" in
    let id_label = id_label label in
    { Value.value =
        Map7
          { t1 = instrument_value t1 ~label
          ; t2 = instrument_value t2 ~label
          ; t3 = instrument_value t3 ~label
          ; t4 = instrument_value t4 ~label
          ; t5 = instrument_value t5 ~label
          ; t6 = instrument_value t6 ~label
          ; t7 = instrument_value t7 ~label
          ; f =
              (fun a b c d e g h ->
                 start_timer id_label;
                 let r = f a b c d e g h in
                 stop_timer id_label;
                 r)
          }
    ; here
    }
;;

let rec instrument_computation'
  : type model action result.
    (model, action, result) Computation.t
    -> start_timer:(string -> unit)
    -> stop_timer:(string -> unit)
    -> current_label:string
    -> (model, action, result) Computation.t
  =
  fun computation ~start_timer ~stop_timer ~current_label ->
  let computation_id = Value_and_computation_id.create () in
  let total_label = total_label ~current_label in
  let id_label = id_label ~id:computation_id in
  let compute_label label = id_label [%string "%{label}-compute"] in
  let apply_action_label label = id_label [%string "%{label}-apply_action"] in
  let by_label label = id_label [%string "%{label}-by"] in
  let time_apply_action ~apply_action ~label ~inject ~schedule_event input model action =
    let label = apply_action_label label in
    start_timer label;
    let model = apply_action ~inject ~schedule_event input model action in
    stop_timer label;
    model
  in
  let instrument_computation' ~label =
    instrument_computation' ~start_timer ~stop_timer ~current_label:label
  in
  let instrument_value ~label =
    instrument_value ~start_timer ~stop_timer ~current_label:label
  in
  let instrument_packed packed ~label =
    let (Computation.T { t; action; model }) = packed in
    let t = instrument_computation' t ~label in
    Computation.T { t; action; model }
  in
  match computation with
  | Path | Fetch _ -> computation
  | Return value ->
    let label = total_label "return" in
    Return (instrument_value value ~label)
  | Leaf1 { input; apply_action; compute; name; kind } ->
    let label = total_label "leaf" in
    let compute_label = compute_label label in
    Leaf1
      { input = instrument_value input ~label
      ; apply_action = time_apply_action ~apply_action ~label
      ; name
      ; kind
      ; compute =
          (fun ~inject input model ->
             start_timer compute_label;
             let computed = compute ~inject input model in
             stop_timer compute_label;
             computed)
      }
  | Leaf0 { apply_action; compute; name; kind } ->
    let label = total_label "leaf0" in
    let compute_label = compute_label label in
    Leaf0
      { apply_action = time_apply_action ~apply_action ~label
      ; name
      ; kind
      ; compute =
          (fun ~inject model ->
             start_timer compute_label;
             let computed = compute ~inject model in
             stop_timer compute_label;
             computed)
      }
  | Leaf_incr { input; apply_action; compute; name } ->
    let label = total_label "leaf_incr" in
    let apply_action_label = apply_action_label label in
    let compute_label = compute_label label in
    Leaf_incr
      { input = instrument_value input ~label
      ; apply_action =
          (fun input ~inject ->
             start_timer apply_action_label;
             let model_incr = apply_action input ~inject in
             stop_timer apply_action_label;
             model_incr)
      ; compute =
          (fun clock input model ~inject ->
             start_timer compute_label;
             let computed = compute clock input model ~inject in
             stop_timer compute_label;
             computed)
      ; name
      }
  | Model_cutoff { t; model } ->
    let label = total_label "model_cutoff" in
    Model_cutoff { t = instrument_computation' t ~label; model }
  | Subst { from; via; into; here } ->
    let label = total_label "subst" in
    Subst
      { from = instrument_computation' from ~label:[%string "%{label}(from)"]
      ; via
      ; into = instrument_computation' into ~label:[%string "%{label}(into)"]
      ; here
      }
  | Subst_stateless { from; via; into; here } ->
    let label = total_label "subst" in
    Subst_stateless
      { from = instrument_computation' from ~label:[%string "%{label}(from)"]
      ; via
      ; into = instrument_computation' into ~label:[%string "%{label}(into)"]
      ; here
      }
  | Store { id; value; inner } ->
    let label = total_label "store" in
    Store
      { id
      ; value = instrument_value value ~label
      ; inner = instrument_computation' inner ~label
      }
  | Assoc
      { map
      ; key_compare
      ; key_id
      ; data_id
      ; by
      ; model_info
      ; action_info
      ; input_by_k
      ; result_by_k
      ; model_by_k
      } ->
    let label = total_label "assoc" in
    Assoc
      { map = instrument_value map ~label
      ; key_compare
      ; key_id
      ; data_id
      ; by = instrument_computation' ~label by
      ; model_info
      ; action_info
      ; input_by_k
      ; result_by_k
      ; model_by_k
      }
  | Assoc_simpl
      { map; key_id; data_id; by; model_info; input_by_k; result_by_k; model_by_k } ->
    let label = total_label "assoc_simpl" in
    let by_label = by_label label in
    Assoc_simpl
      { map = instrument_value map ~label
      ; key_id
      ; data_id
      ; by =
          (fun path key value ->
             start_timer by_label;
             let by = by path key value in
             stop_timer by_label;
             by)
      ; model_info
      ; input_by_k
      ; result_by_k
      ; model_by_k
      }
  | Enum { which; out_of; sexp_of_key; key_equal; key_compare; key_type_id; key_and_cmp }
    ->
    let label = total_label "enum" in
    Enum
      { which = instrument_value which ~label
      ; out_of = Map.map out_of ~f:(instrument_packed ~label)
      ; sexp_of_key
      ; key_equal
      ; key_compare
      ; key_type_id
      ; key_and_cmp
      }
  | Lazy packed ->
    let label = total_label "lazy" in
    Lazy (Lazy.map packed ~f:(instrument_packed ~label))
  | Wrap { model_id; inject_id; inner; apply_action } ->
    let label = total_label "wrap" in
    Wrap
      { model_id
      ; inject_id
      ; inner = instrument_computation' inner ~label
      ; apply_action = time_apply_action ~apply_action ~label
      }
  | With_model_resetter { t; default_model } ->
    let label = total_label "with_model_resetter" in
    With_model_resetter { t = instrument_computation' t ~label; default_model }
  | Lifecycle value ->
    let label = total_label "life_cycle" in
    Lifecycle (instrument_value value ~label)
;;

let instrument_computation
      (Computation.T { t; model; action } : 'a Computation.packed)
      ~start_timer
      ~stop_timer
  =
  Computation.T
    { t = instrument_computation' t ~start_timer ~stop_timer ~current_label:""
    ; model
    ; action
    }
;;
