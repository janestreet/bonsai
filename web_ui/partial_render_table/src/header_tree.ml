open! Core
open! Bonsai_web

type t =
  | Leaf of leaf
  | Group of group
  | Organizational_group of t list
  | Spacer of t

and leaf =
  { leaf_header : (Vdom.Node.t[@sexp.opaque])
  ; initial_width : Css_gen.Length.t
  ; visible : bool
  }

and group =
  { children : t list
  ; group_header : (Vdom.Node.t[@sexp.opaque])
  }
[@@deriving sexp]

let rec colspan = function
  | Leaf { visible = true; _ } -> 1
  | Leaf { visible = false; _ } -> 0
  | Organizational_group children | Group { children; _ } ->
    List.sum (module Int) children ~f:colspan
  | Spacer t -> colspan t
;;

let rec height = function
  | Leaf _ -> 1
  | Organizational_group children -> height_of_many children
  | Group { children; _ } -> height_of_many children + 1
  | Spacer child -> 1 + height child

and height_of_many children =
  children
  |> List.map ~f:height
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0
;;

let rec leaves = function
  | Leaf leaf -> [ leaf ]
  | Spacer t -> leaves t
  | Organizational_group children | Group { children; _ } ->
    List.concat_map children ~f:leaves
;;

let column_names t =
  let results = ref [] in
  let rec acc list node =
    match node with
    | Leaf { leaf_header; _ } -> results := (leaf_header :: list |> List.rev) :: !results
    | Spacer s -> acc list s
    | Organizational_group ts -> List.iter ts ~f:(acc list)
    | Group { group_header; children } ->
      List.iter children ~f:(acc (group_header :: list))
  in
  acc [] t;
  List.rev !results
;;

let leaf ~header:leaf_header ~initial_width ~visible =
  Leaf { leaf_header; initial_width; visible }
;;

let spacer t = Spacer t

let balance children =
  let max_height = height_of_many children in
  List.map children ~f:(fun child ->
    let child_height = height child in
    Fn.apply_n_times ~n:(max_height - child_height) spacer child)
;;

let org_group children =
  let balanced = balance children in
  Organizational_group balanced
;;

let group ~header:group_header children =
  let children = balance children in
  Group { children; group_header }
;;
