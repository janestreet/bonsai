open! Core
open! Import


module Var_from_parent : sig
  type t =
    | None
    (** The common case, in which the parent node does not introduce any variables. *)
    | One of Type_equal.Id.Uid.t
    (** The case in which the parent node introduces one new variable; most of
        the time this is for [Subst] or [Subst_stateless] *)
    | Two of Type_equal.Id.Uid.t * Type_equal.Id.Uid.t
    (** The case in which the parent node introduces two new variables; most of
        the time this is for [Assoc]. *)
end

(** Both [For_value.map] and [For_computation.map] involve interacting
    universally quantified functions. Since OCaml does not support first-class
    universally quantified functions, so we have to wrap the function in
    a single-field record (we have done this twice in this module).

    The user-supplied mapping function receives:
    - a [mapper] that it should call to continue descending into the computation/value.
    - possibly an id representing the variable to which the current node will
      be bound (that is, the current node is either the [from] field of a [subst]
      or the [inner] field of a [store].
    - something called [var_from_parent], which is the uid of any type id
      contained which introduces a value from the immediate parent node. This
      will be [Some] if the immediate parent is a [Subst], [Subst_stateless],
      or [Store] node, since those each introduce a new type id into scope.
      This parameter can be used to build up a mapping from type id to the
      computation that type id represents.
    - a value passed down from its direct parent. This is not quite the same
      concept as a fold accumulator, although it is similar (you could think of
      it as the accumulator for folding over a particular path through the tree.
    - the current node in the tree.

    The mapping function should:
    - transform the current node in some way.
    - call the [mapper] with the its info for the child and the current node.

    Note that you could invoke the mapper either before or after transforming
    the current node (these correspond to post- and pre- order traversal). *)
module For_value : sig
  type 'from_parent mapper = { f : 'a. 'from_parent -> 'a Value.t -> 'a Value.t }

  type 'from_parent user_mapper =
    { f :
        'a.
          recurse:'from_parent mapper
        -> var_from_parent:Var_from_parent.t
        -> parent_path:Node_path.t Lazy.t
        -> current_path:Node_path.t Lazy.t
        -> 'from_parent
        -> 'a Value.t
        -> 'a Value.t
    }
end

module For_computation : sig
  type 'from_parent mapper =
    { f :
        'model 'dynamic_action 'static_action 'result.
          'from_parent
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
    }

  type 'from_parent user_mapper =
    { f :
        'model 'dynamic_action 'static_action 'result.
          recurse:'from_parent mapper
        -> var_from_parent:Var_from_parent.t
        -> parent_path:Node_path.t Lazy.t
        -> current_path:Node_path.t Lazy.t
        -> 'from_parent
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
        -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
    }
end

val map
  :  computation_mapper:'from_parent For_computation.user_mapper
  -> value_mapper:'from_parent For_value.user_mapper
  -> init:'from_parent
  -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
  -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
