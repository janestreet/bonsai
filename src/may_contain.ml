open! Core

module T = struct
  type t =
    | Yes_or_maybe
    | No

  let merge a b =
    match a, b with
    | Yes_or_maybe, _ | _, Yes_or_maybe -> Yes_or_maybe
    | No, No -> No
  ;;
end

module Lifecycle = T
module Path = T
