open! Core
open Bonsai_web

type ('a, 'view) t =
  { value : 'a Or_error.t
  ; view : 'view
  ; set : 'a -> unit Vdom.Effect.t
  }

(** [return] produces a bonsai form that will always produce the same value.
    [set] and [normalize] will do nothing to the form provided by this. *)
val return : ?sexp_of_t:('a -> Sexp.t) -> 'a -> ('a, unit) t

(** [return_settable] is identical to [return], but [set] and [normalize] will update
    the value of the form. *)
val return_settable
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a
  -> ('a, unit) t Computation.t

(** [return_error] produces a form that always fails validation. *)
val return_error : Error.t -> (_, unit) t

val map_error : ('a, 'view) t -> f:(Error.t -> Error.t) -> ('a, 'view) t
val value : ('a, _) t -> 'a Or_error.t
val value_or_default : ('a, _) t -> default:'a -> 'a

(** [is_valid] returns true if [value] would return [Ok]. *)
val is_valid : _ t -> bool

val view : (_, 'view) t -> 'view
val map_view : ('a, 'view1) t -> f:('view1 -> 'view2) -> ('a, 'view2) t

(** [set] fills the form with the provided value, setting the contents of
    form-elements if possible *)
val set : ('a, _) t -> 'a -> unit Ui_effect.t

(** [normalize] sets the contents of a form to its current value.  This only
    impacts values that have a "normalized" form.  For example, a
    float-producing textbox being normalized might go from displaying "1.000"
    to "1." *)
val normalize : _ t -> unit Ui_effect.t

(** Combines two forms into another one that produces both values from the
    inputs in tupled form. *)
val both : ('a, 'view1) t -> ('b, 'view2) t -> ('a * 'b, 'view1 * 'view2) t

(** Combines a list of forms into another that produces all values from the inputs in
    list form. *)
val all : ('a, 'view) t list -> ('a list, 'view list) t

(** Combines a map of forms into another that produces all values from the inputs in
    map form. *)
val all_map
  :  ('k, ('a, 'view) t, 'cmp) Map.t
  -> (('k, 'a, 'cmp) Map.t, ('k, 'view, 'cmp) Map.t) t

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
    ]} 

    [extend_view_with_error] is only called with errors that come from _this_ call 
    to [project] *)
val project
  :  ?extend_view_with_error:('view -> Error.t -> 'view)
  -> ('a, 'view) t
  -> parse_exn:('a -> 'b)
  -> unparse:('b -> 'a)
  -> ('b, 'view) t

(** The same as [project] except that the [parse] function is [Or_error] returning. 

    [extend_view_with_error] is only called with errors that come from _this_ call 
    to [project'] *)
val project'
  :  ?extend_view_with_error:('view -> Error.t -> 'view)
  -> ('a, 'view) t
  -> parse:('a -> 'b Or_error.t)
  -> unparse:('b -> 'a)
  -> ('b, 'view) t

(** [validate] can provide additional validation of a value, but unlike [project] or
    [project'], it doesn't change the type of the resulting form 

    [extend_view_with_error] is only called with errors that come from _this_ call 
    to [validate] *)
val validate
  :  ?extend_view_with_error:('view -> Error.t -> 'view)
  -> ('a, 'view) t
  -> f:('a -> unit Or_error.t)
  -> ('a, 'view) t

(** [optional] takes a [('a, 'view) t] and produces a ['a option t] when given a "some detector"
    and a token "none" value. [is_some none] must be false.

    Example:
    {[
      let _ : string option t =
        optional
          (a: string t)
          ~is_some:(Fn.non String.is_empty)
          ~none:""
    ]} *)
val optional : ('a, 'view) t -> is_some:('a -> bool) -> none:'a -> ('a option, 'view) t

(** An alternative "optional form" construction function; [optional']
    gives you the ability to produce the full set of parse options:
    - Ok (Some b)
    - Ok None
    - Error error]
      while also converting to another type (['a -> 'b option]) at the same time. *)
val optional'
  :  ('a, 'view) t
  -> parse:('a -> 'b option Or_error.t)
  -> unparse:('b -> 'a)
  -> none:'a
  -> ('b option, 'view) t

(** [fallback_to] modifies the given form so that [Ok default] is used as the form's
    value when it is in an [Error] state. *)
val fallback_to : ('a, 'view) t -> value:'a -> ('a, 'view) t

(** Unlike the rest of the API which operates on
    values of type [Form.t] value values, they operate on [Form.t
    Value.t], and typically return [Computation.t]. *)
module Dynamic : sig
  (* Sets a [('a, 'view) t] to a default value the first time it is displayed on a page.
     [with_default] does not set the model a second time if the form is removed
     from the page and then re-added. *)
  val with_default : 'a Value.t -> ('a, 'view) t Value.t -> ('a, 'view) t Computation.t

  (* Like [with_default], but an effect is provided to produce the default value. *)
  val with_default_from_effect
    :  'a Effect.t Value.t
    -> ('a, 'view) t Value.t
    -> ('a, 'view) t Computation.t

  (* Sets a [('a, 'view) t] to an initial value every time it is (re)displayed on a page. *)
  val with_default_always
    :  'a Value.t
    -> ('a, 'view) t Value.t
    -> ('a, 'view) t Computation.t

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
    -> ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> f:('a -> unit Ui_effect.t) Value.t
    -> ('a, 'view) t Value.t
    -> unit Computation.t

  (** Synchronizes a form with some external storage. The value from the store is used to
      initially populate the form, and if the form changes, then the store is updated to
      match.  If the contents of the store changes, the form is also updated to match.
      If both the form and the store update at the same time, the form wins, and the store
      is overridden. *)
  val sync_with
    :  ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> store_value:'a option Value.t
    -> store_set:('a -> unit Effect.t) Value.t
    -> ('a, 'view) t Value.t
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
    :  ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> ?one_at_a_time:bool
    -> ?debounce_ui:Time_ns.Span.t
    -> ('a, 'view) t Value.t
    -> f:('a -> unit Or_error.t Effect.t) Value.t
    -> ('a, 'view) t Computation.t

  (** This works the same as [validate_via_effect], but keeps track of the result and
      uses that as the value for the form. *)
  val project_via_effect
    :  ?sexp_of_input:('a -> Sexp.t)
    -> ?sexp_of_result:('b -> Sexp.t)
    -> equal_input:('a -> 'a -> bool)
    -> equal_result:('b -> 'b -> bool)
    -> ?one_at_a_time:bool
    -> ?debounce_ui:Time_ns.Span.t
    -> ('a, 'view) t Value.t
    -> unparse:('b -> 'a)
    -> parse:('a -> 'b Or_error.t Effect.t) Value.t
    -> ('b, 'view) t Computation.t
end
