open! Core
open! Bonsai_web

type 'column_id t = private
  | Leaf of 'column_id leaf
  | Group of 'column_id group
  | Organizational_group of 'column_id t list
  | Spacer of 'column_id t

and 'column_id leaf = private
  { leaf_header : Vdom.Node.t
  ; initial_width : Css_gen.Length.t
  ; visible : bool
  ; column_id : 'column_id
  }

and 'column_id group = private
  { children : 'column_id t list
  ; group_header : Vdom.Node.t
  }
[@@deriving sexp]

val leaf
  :  header:Vdom.Node.t
  -> initial_width:Css_gen.Length.t
  -> visible:bool
  -> column_id:'column_id
  -> 'column_id t

val group : header:Vdom.Node.t -> 'column_id t list -> 'column_id t
val org_group : 'column_id t list -> 'column_id t
val colspan : _ t -> int
val height : _ t -> int
val leaves : 'column_id t -> 'column_id leaf list

(** For each leaf, [column_names] returns a list like [group_header; group_header; ...;
    leaf_header], where the group labels are that leaf's ancestors, ordered left to right
    from most to least distant. Used for rendering column groups in tests. *)
val column_names : _ t -> Vdom.Node.t list list
