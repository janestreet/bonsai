open! Core
module View = View
module Form_view := View
open Bonsai_web

type 'a t

(** [return] produces a bonsai form that will always produce the same value.
    [set] and [normalize] will do nothing to the form provided by this. *)
val return : 'a -> 'a t

(** [return_settable] is identical to [return], but [set] and [normalize] will update
    the value of the form. *)
val return_settable : (module Bonsai.Model with type t = 'a) -> 'a -> 'a t Computation.t

(** [return_error] produces a form that always fails validation. *)
val return_error : Error.t -> _ t

val map_error : 'a t -> f:(Error.t -> Error.t) -> 'a t
val value : 'a t -> 'a Or_error.t
val value_or_default : 'a t -> default:'a -> 'a

(** [is_valid] returns true if [value] would return [Ok]. *)
val is_valid : _ t -> bool

val view : _ t -> Form_view.t
val map_view : 'a t -> f:(Form_view.t -> Form_view.t) -> 'a t

module Submit : sig
  type 'a t = private
    { f : 'a -> unit Ui_effect.t
    ; handle_enter : bool
    ; button_text : string option
    ; button_attr : Vdom.Attr.t
    }

  (** Creates a "submit" handler, which is intended to be used by the [view_as_vdom] function.

      - [handle_enter]: when true, will render the form
        inside of a <form> element, which gives us the ability to add an
        "on_submit" handler which detecs people hitting "enter" when a form element
        is focused.  The default is [true].  Set it to [false] to disable this behavior.
      - [button]: When Some, will append a button with the label given by its
        contents.  will be added to the end of the form.  If the form is
        currently invalid, the button will be disabled.  The default is [Some
        "Submit"].  Explicitly set it to [None] to remove the button entirely.
      - [button_attr]: A [Vdom.Attr.t] to attach to the submit button.
      - [f]: the function which is run when the form is submitted.  *)

  val create
    :  ?handle_enter:bool
    -> ?button:string option
    -> ?button_attr:Vdom.Attr.t
    -> f:('a -> unit Ui_effect.t)
    -> unit
    -> 'a t
end

(** [view_as_vdom] produces the vdom representation of the form.

    [editable] defaults to [`Yes_always], which should be used when form input can't be
    disabled. [`Currently_yes] allows editing, but generates less diff when toggled with
    [`Currently_no]. When [editable] is [`Currently_no], the view is wrapped in a fieldset
    that disables all of the inputs in the form.

    Regardless of the value of [editable], scheduling the [Form.set] effect
    will still change the values in the form.

    Known bugs:
    While setting editable to `Currently_no prevents modification of most
    browser-builtin input elements, some custom form elements like the
    drag-and-drop, multiselect, and removing items using the pills in
    typeahead-multi for don't currently respect this and can be modified anyway.
    Work is underway to fix these. *)
val view_as_vdom
  :  ?theme:View.Theme.t
  -> ?on_submit:'a Submit.t
  -> ?editable:Form_view.editable
  -> 'a t
  -> Vdom.Node.t

(** [set] fills the form with the provided value, setting the contents of
    form-elements if possible *)
val set : 'a t -> 'a -> unit Ui_effect.t

(** [normalize] sets the contents of a form to its current value.  This only
    impacts values that have a "normalized" form.  For example, a
    float-producing textbox being normalized might go from displaying "1.000"
    to "1." *)
val normalize : _ t -> unit Ui_effect.t

(** Combines two forms into another one that produces both values from the
    inputs in tupled form. *)
val both : 'a t -> 'b t -> ('a * 'b) t

(** Combines a list of forms into another that produces all values from the inputs in
    list form. *)
val all : 'a t list -> 'a list t

(** Combines a map of forms into another that produces all values from the inputs in
    map form. *)
val all_map : ('k, 'a t, 'cmp) Map.t -> ('k, 'a, 'cmp) Map.t t

(** [project] is the powerhouse of the library; Using this function, you
    can change the type produced. Think of it like [map].

    - [parse_exn] is a function that converts "forwards".  As its name implies,
      you're free (and encouraged to) throw exceptions when the type conversion
      would fail.
    - [unparse] goes in the opposite direction.  This one must not throw.

    Example:

    {[
      let _ : int Form.t =
        project
          (a: string Form.t)
          ~parse_exn:Int.of_string
          ~unparse:Int.to_string
    ]} *)
val project : 'a t -> parse_exn:('a -> 'b) -> unparse:('b -> 'a) -> 'b t

(** The same as [project] except that the [parse] function is [Or_error] returning. *)
val project' : 'a t -> parse:('a -> 'b Or_error.t) -> unparse:('b -> 'a) -> 'b t

(** [validate] can provide additional validation of a value, but unlike
    [project] or [project'], it doesn't
    change the type of the resulting form *)
val validate : 'a t -> f:('a -> unit Or_error.t) -> 'a t

(** Adds a label to the form. *)
val label : string -> 'a t -> 'a t

(** Same as [label], but it lets you use an arbitrary vdom node instead of just a string. *)
val label' : Vdom.Node.t -> 'a t -> 'a t

(** Adds a tooltip to the form. *)
val tooltip : string -> 'a t -> 'a t

(** Same as [tooltip], but it lets you use an arbitrary vdom node instead of just a string. *)
val tooltip' : Vdom.Node.t -> 'a t -> 'a t

(** [optional] takes a ['a t] and produces a ['a option t] when given a "some detector"
    and a token "none" value. [is_some none] must be false.

    Example:
    {[
      let _ : string option t =
        optional
          (a: string t)
          ~is_some:(Fn.non String.is_empty)
          ~none:""
    ]} *)
val optional : 'a t -> is_some:('a -> bool) -> none:'a -> 'a option t

(** An alternative "optional form" construction function; [optional']
    gives you the ability to produce the full set of parse options:
    - Ok (Some b)
    - Ok None
    - Error error]
      while also converting to another type (['a -> 'b option]) at the same time. *)
val optional'
  :  'a t
  -> parse:('a -> 'b option Or_error.t)
  -> unparse:('b -> 'a)
  -> none:'a
  -> 'b option t


(** [fallback_to] modifies the given form so that [Ok default] is used as the form's
    value when it is in an [Error] state. *)
val fallback_to : 'a t -> value:'a -> 'a t

(** [Record_builder] is the primary way to compose form values using this library.

    Example:
    {[

      type my_type =
        { x : string
        ; y : int
        } [@@deriving fields]


      val a : string t
      val b : int t

      let c: my_type t =
        let open Form.Record_builder in
        build_for_record
          (Fields.make_creator
             ~x:(field a)
             ~y:(field b))
    ]} *)
module Record_builder :
  Record_builder_intf.Record_builder with type 'a profunctor_term := 'a t

(** Unlike the rest of the API which operates on
    values of type [Form.t] value values, they operate on [Form.t
    Value.t], and typically return [Computation.t]. *)
module Dynamic : sig
  (* Sets a ['a t] to a default value the first time it is displayed on a page.
     [with_default] does not set the model a second time if the form is removed
     from the page and then re-added. *)
  val with_default : 'a Value.t -> 'a t Value.t -> 'a t Computation.t

  (* Like [with_default], but an effect is provided to produce the default value. *)
  val with_default_from_effect : 'a Effect.t Value.t -> 'a t Value.t -> 'a t Computation.t

  (* Sets a ['a t] to an initial value every time it is (re)displayed on a page. *)
  val with_default_always : 'a Value.t -> 'a t Value.t -> 'a t Computation.t

  (** Adds a clickable error hint for this form  *)
  val error_hint : 'a t Value.t -> 'a t Computation.t

  (** Adds a group that is clickable.  Visibility for sub-forms is initially
      determined by [starts_open] but is toggled by clicking on the label. *)
  val collapsible_group
    :  ?starts_open:bool
    -> string Value.t
    -> 'a t Value.t
    -> 'a t Computation.t

  (** Adds a on_change handler to a [Form.t].

      The function [f] is called when
      1. the (successfully validated) value of the form changes.
      2. the form transitions from producing an Error to producing a value.
      3. the form initializes with a value

      [on_error] is called when
      1. the error message changes
      2. the form transitions from producing a value to an error
      3. the form initializes with an error *)
  val on_change
    :  ?on_error:(Error.t -> unit Ui_effect.t) Value.t
    -> (module Bonsai.Model with type t = 'a)
    -> f:('a -> unit Ui_effect.t) Value.t
    -> 'a t Value.t
    -> unit Computation.t

  (** Synchronizes a form with some external storage. The value from the store is used to
      initially populate the form, and if the form changes, then the store is updated to
      match.  If the contents of the store changes, the form is also updated to match.
      If both the form and the store update at the same time, the form wins, and the store
      is overridden. *)
  val sync_with
    :  (module Bonsai.Model with type t = 'a)
    -> store_value:'a option Value.t
    -> store_set:('a -> unit Effect.t) Value.t
    -> 'a t Value.t
    -> unit Computation.t


  (** Unlike [validate] which requires the validation function to be available
      locally (and synchronous), [validate_via_effect] runs an effectful computation.
      The asynchrony makes this function interesting:

      When a value is in the midst of validated, the resultant form is resolved
      to an Error.

      [one_at_a_time] (defaults to false) when set to true, will use
      Bonsai.Effect_throttling.poll to make sure that only one instance of the effect

      [debounce_ui] can be set to a timespan.  When set, the validation status won't
      update until the input form value has been stable for the given span of time *)
  val validate_via_effect
    :  (module Bonsai.Model with type t = 'a)
    -> ?one_at_a_time:bool
    -> ?debounce_ui:Time_ns.Span.t
    -> 'a t Value.t
    -> f:('a -> unit Or_error.t Effect.t) Value.t
    -> 'a t Computation.t

  module Record_builder :
    Record_builder_intf.Dynamic_record_builder with type 'a profunctor_term := 'a t
end

module Expert : sig
  val create
    :  value:'a Or_error.t
    -> view:Form_view.t
    -> set:('a -> unit Ui_effect.t)
    -> 'a t
end

module Private : sig
  val suggest_label : string -> 'a t -> 'a t
end
