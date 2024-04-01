open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var

module My_google_clone = struct
  (* $MDX part-begin=type *)
  type t =
    | Homepage
    | Search of string
  [@@deriving sexp, equal]
  (* $MDX part-end *)

  (* $MDX part-begin=parse_unparse *)
  let parse_exn ({ path; query; _ } : Url_var.Components.t) : t =
    let path = String.split path ~on:'/' in
    match path with
    | [ "home" ] -> Homepage
    | [ "search" ] ->
      (match Map.find (query : _ String.Map.t) "q" with
       | Some [ query ] -> Search query
       | None | Some [] -> failwith "search missing query param"
       | Some (_ :: _ :: _) -> failwith "search with too many query params")
    | _ -> failwith "unknown path"
  ;;

  let unparse (t : t) : Url_var.Components.t =
    match t with
    | Homepage -> Url_var.Components.create ~path:"home" ()
    | Search query ->
      Url_var.Components.create
        ~path:"search"
        ~query:(String.Map.singleton "q" [ query ])
        ()
  ;;
  (* $MDX part-end *)
end

let () = ignore My_google_clone.parse_exn
let () = ignore My_google_clone.unparse
