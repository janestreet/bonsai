open! Core
open! Import

(** Traverses a computation to reach the values within. Any values of the form

    {[
      both a (both b c)
    ]}

    are transformed to

    {[
      map3 a b c ~f:(fun a b c -> a, (b, c))
    ]}

    since this generates fewer incremental nodes, thus leading to better
    performance. The same optimization gets applied for up to 7 nodes both'd
    together. A similar optimization is applied to nodes of the form

    {[
      map (both a b) ~f
    ]}

    which get turned into

    {[
      map2 a b ~f:(fun a b -> f (a, b))
    ]}

    since, again, this generates fewer incremental nodes. As with the previous
    transformation, this generalizes to an arity of 7.
*)
val flatten_values
  :  ('model, 'dynamic_action, 'static_action, 'result) Computation.t
  -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
