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
  -> ?set_min_height_to_100vh:unit
  (** Kado set the default background color of [:root]
      to set the background color. This can prove troublesome
      since css variables cannot currently easily be set if
      they are read at the top-level. If you change the background color
      you might most likely also need to change the strategy that kado uses to set the background
      color to be [~set_min_height_to_100vh:()] *)
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
