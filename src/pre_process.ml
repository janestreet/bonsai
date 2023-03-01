open! Core
open! Import

let pre_process t = t |> Constant_fold.constant_fold |> Flatten_values.flatten_values
