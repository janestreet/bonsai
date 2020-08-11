open! Core_kernel
open! Import

module Result_spec : sig
  include module type of struct
    include Bonsai_test.Result_spec
  end

  val vdom : ('a -> Vdom.Node.t) -> ('a, Nothing.t) t
end

module Handle : sig
  include module type of struct
    include Bonsai_test.Handle
  end

  val click_on
    :  ('a, 'b) t
    -> get_vdom:('a -> Virtual_dom.Vdom.Node.t)
    -> selector:string
    -> unit

  val input_text
    :  ('a, 'b) t
    -> get_vdom:('a -> Virtual_dom.Vdom.Node.t)
    -> selector:string
    -> text:string
    -> unit
end
