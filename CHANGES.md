## Release v0.16.0

- Formatting improvements:
  * Remove unnecessary newline in the `Bonsai` module documentation comment
  * Remove extra line in Bonsai documentation
  * Remove line break in the comment for `Computation` type in `Bonsai`

- Update function signatures:
  * Modify `Bonsai.Value.cutoff` to move the `equal` parameter to the end
  * Update `Clock.every` to add `when_to_start_next_effect` and optional `trigger_on_activate` parameters
  * Update `Bonsai.wrap`, `actor0`, and `actor1` functions to include optional `reset` parameter
  * Update `Bonsai.Edge.on_change`, `effect_on_change`, and `on_change'` functions to remove `Source_code_position.t` parameter
  * Update `Let_syntax.sub` function in `Bonsai` to accept `here` parameter
  * Update `Let_syntax.map` function in `Bonsai` to accept an optional `here` parameter
  * Modify `Bonsai.Debug.to_dot` function signature to accept an optional `pre_process` parameter

- Add new functions:
  * `Bonsai.fold_right`
  * `Bonsai.For_open`
  * `Bonsai.Var.incr_var`
  * `Bonsai.Computation_status`
  * `Bonsai.state`, `state_opt`, `state_machine1`, and related functions
  * `Bonsai.toggle`, `Toggle` module, and `toggle'` function
  * `Bonsai.of_module0`, `of_module1`, and `of_module2`
  * `Bonsai.freeze`
  * `Bonsai.scope_model`
  * `Bonsai.most_recent_some`, `most_recent_value_satisfying`, and `previous_value`
  * `Bonsai.assoc_set` and `assoc_list`
  * `Bonsai.with_model_resetter'`
  * `Bonsai.yoink`
  * `Bonsai.sub`
  * `Bonsai.cutoff`, `switch`, and `map` functions in `Let_syntax`
  * `Bonsai.Debug.on_change` and `on_change_print_s`
  * `Bonsai.Expert`, `Map`, and `Stable` modules

- Remove `model_cutoff` function
