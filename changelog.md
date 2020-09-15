# Bonsai Changelog
<!-- Change-log notes go in this file, with newer dates at the _top_  --> 

## 2020-08-13
Added `Bonsai.state_opt`, like state, but storing an option of a given model.

## 2020-07-27
Added `Bonsai.lazy`, a function permitting recursive components.

## 2020-07-20
Only the `Value.t` produced from `Let_syntax.both` are collapsed.

## 2020-07-14
Added `Value.cutoff`, a function for specifying cutoff to a greater degree than 
otherwise available.

## 2020-07-04
- Added assoc-optimization and the computation simplifier

## 2020-07-03
- Fixed memory leak in Bonsai.assoc with models not being removed 
  when their values are set back to the default.

## 2020-06-25
Bonsai Template generator added.

## 2020-06-10
`Bonsai.Proc` module added.  To read more, check out
[this document](./docs/proc.md).

## 2020-03-17
Model type removed from `('input, 'model, 'result) Bonsai.t`.

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
