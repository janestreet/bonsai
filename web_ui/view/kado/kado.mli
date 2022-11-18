open! Core
open! Import

module Style : sig
  type t = Dark
end

module Contrast : sig
  type t = Standard
end

module Version : sig
  type t =
    | V1
    | Bleeding
end

val theme
  :  ?contrast:Contrast.t
  -> ?style:Style.t
  -> version:Version.t
  -> unit
  -> View.Theme.t
