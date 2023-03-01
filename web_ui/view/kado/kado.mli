open! Core
open! Import

module Style : sig
  type t =
    | Light
    | Dark
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

module Unstable : sig
  module Buttons : sig
    val vertical_group : Vdom.Attr.t
    val horizontal_group : Vdom.Attr.t
    val small : Vdom.Attr.t
    val thinking : Vdom.Attr.t
    val pressed : Vdom.Attr.t
  end

  module Input = Input
end
