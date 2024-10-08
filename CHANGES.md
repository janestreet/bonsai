# Release v0.17.0
- Rewrote entire bonsai API. The new version is inside of the `Bonsai.Cont` namespace.
  new docs here: https://github.com/janestreet/bonsai/tree/master/docs/guide

- Added a `duplicate` button for elements within auto-generated list forms.

- Added a function to resize prt colum widths.

- Add `Bonsai.Value.of_opt` to new `Bonsai.Cont` api.

- Migrated bonsai examples to new `Bonsai.Cont` API.

- Add documentation about changes and upgrade strategies between proc and cont

- Add 3 new APIs to close Bonsai notifications:
  * `close_all_notifications` closes all currently open notifications
  * `close_oldest_notification` closes the oldest currently open notification
  * `close_newest_notification` closes the newest currently open notification

- Rename `Bonsai.Cont.yoink` to `Bonsai.Cont.peek` to mirror the peek operation in
other abstract data types



- Made the `close_when_clicked_outside` argument to `Bonsai_web_ui_popover` dynamic.

- Changed Bonsai path generation so that paths are only extended at branch points where
multple children might reference the paths.

- Balance long chains of Sub nodes to prevent stack overflows and suboptimal
linear chains of incremental model values.

- Added effects to lock and unlock focus in the `Bonsai_web_ui_partial_render_table`

- `Bonsai_web_ui_form` and `Bonsai_web_ui_form2` were merged into `Bonsai_web_ui_form` under
submodules `With_automatic_view` and `With_manual_view`, respectively.

- Added `always_update_when_focused` to text inputs to make the current behavior (text value can't be updated if the input is focused) optional.

- Remove the first-class Action module from
  * `Bonsai_extra.state_machine0_dynamic_model` and
  * `Bonsai_extra.state_machine1_dynamic_model`
  in favor of an optional `sexp_of_action: 'action -> Sexp.t` parameter.

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
