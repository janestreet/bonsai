# Bonsai Changelog
<!-- Change-log notes go in this file, with newer dates at the _top_  -->
<!-- in vim : !date +'\%Y-\%m-\%d' -->

## 2022-04-01
- Moved `incr_map` wrapper functions from `bonsai_extra` to `bonsai`. They now
  live in the `Bonsai.Map` namespace.

## 2022-02-02
- Added first version of `Bonsai_web_ui_sexp` to generate UIs/forms from Sexp grammars

## 2022-01-12
- Expose `item_is_hovered` to the `empty_list_placeholder` parameter in the
  `Bonsai_web_ui_reorderable_list` API.

## 2021-12-21
- Introduce both `startBonsaiDebugger` and `stopBonsaiDebugger` functions to
  global scope. Starting the debugger instruments the Bonsai graph and opens a
  new tab in bonsai-bug.

## 2021-12-14
- Added `?expand_direction` parameter to `Bonsai_web_ui_query_box` to have
  suggestions appear above the input textbox.

## 2021-11-01
- Added `Bonsai_bench`

## 2021-07-20
- Added `Bonsai_web_ui_reorderable_list.{simple,with_inject}`

## 2021-06-11
- Added `Form.validate_via_effect`

## 2021-06-07
- Added `Bonsai_extra`

## 2021-06-02
- Added `Bonsai.lazy_` back

## 2021-05-12
- Added dynamic scoping

## 2021-04-13
- Added `Bonsai_web_ui_not_connected_warning_box`

## 2021-03-17
- Added form submission options to `Bonsai_web_ui_form`

## 2021-03-16
- Added `Bonsai_web_ui_codemirror`

## 2021-02-17
- Added `Bonsai_web_ui_form.Elements.File_select`, for picking single
  or multiple files.
- Added `Bonsai_web_ui_file`, for reading files from disk once
  selected.

## 2021-02-11
- Change `should_print_styles` to the more general `filter_printed_attributes`

## 2021-01-21
- Added `Bonsai.Clock.approx_now` and `Bonsai.Clock.now` and moved
  `Bonsai.Edge.every` into the `Clock` module

## 2021-01-19
- Added `Bonsai.Incr.with_clock` and a parameter to `Handle.create` to allow easier
  testing of time-sensitive components.
- Removed `Bonsai.lazy_`

## 2021-01-15
- Added `?split` parameter to `Bonsai_web_ui_typeahead.create_multi` to allow
  adding multiple items with a single input.

## 2020-12-30
- Deprecated `Bonsai.if_`, `Bonsai.match_either`, `Bonsai.match_result`, and
  `Bonsai.match_option` because `match%sub` expressions are always preferred.
- Renamed `Bonsai_web_ui_drag_and_drop.universe` to `create`

## 2020-11-30
- Added `Bonsai_web_ui_drag_and_drop` module for making UIs with drag-and-drop.

## 2020-12-09
- Added `lib/bonsai/web_ui/extendy`, a component for interactively adding
  and removing components.
- Added `Bonsai_web_ui_url_var`, a Var for urls.

## 2020-11-18
- Added `Bonsai.Edge.Poll`, which polls an input variable, and schedules an
  effect when it changes.
- Added `Bonsai.Edge.every` for scheduling events at periodic intervals.

## 2020-11-11
- Added `Bonsai.actor` for communicating between components.

## 2020-10-19
- Added `Bonsai.Edge` for edge triggering computations.

## 2020-10-06
- Added `Bonsai_web.Persistent_var` for defining a Var that saves its
contents in either local-storage or session storage.

## 2020-09-29
- Added `Bonsai.Incr`, a module for interacting more directly with the
incremental implementation of Bonsai.

## 2020-09-25
- Added `Handle.show_diff` for testing, which prints the diff between
the output from the last call to `show` or `show_diff` and the current
output.

## 2020-08-13
- Added `Bonsai.state_opt`, like state, but storing an option of a
  given model.

## 2020-07-27
- Added `Bonsai.lazy`, a function permitting recursive components.

## 2020-07-20
- Only the `Value.t` produced from `Let_syntax.both` are collapsed.

## 2020-07-14
- Added `Value.cutoff`, a function for specifying cutoff to a greater
degree than otherwise available.

## 2020-07-04
- Added assoc-optimization and the computation simplifier

## 2020-07-03
- Fixed memory leak in Bonsai.assoc with models not being removed
  when their values are set back to the default.

## 2020-06-25
- Bonsai Template generator added.

## 2020-06-10
`Bonsai.Proc` module added.  To read more, check out
[this document](./docs/blog/proc.md).

## 2020-03-17
- Model type removed from `('input, 'model, 'result) Bonsai.t`.

## 2020-02-27
- Changelog file created
- Refactored bonsai library to have an incrementally generic interface
- Added `Bonsai.input` and `Bonsai.model` functions, replacing `Bonsai.id`
- Added `Bonsai.Model.state_machine`
- Added "extendable list" example
- Made optimization engine smarter
- Shortened `Bonsai.Project.Model` down to `Bonsai.Model`
- Removed `Bonsai.Incremental.switch` and related modules
- Renamed `Bonsai.Incremental` to `Bonsai.With_incr`
- Added `Bonsai.With_incr.value_cutoff`
