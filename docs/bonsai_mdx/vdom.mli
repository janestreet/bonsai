module Event : sig
  type t

  val no_op : t
  val sequence : t list -> t
end

module Attr : sig
  type t

  val on_click : (_ -> Event.t) -> t
end

module Node : sig
  type t

  val button : Attr.t list -> t list -> t
  val text : string -> t
  val div : Attr.t list -> t list -> t
end
