open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var
open Url_var.Typed
module Form = Bonsai_web_ui_form
module Path_order = Url_var.Typed.Parser.Record.Path_order

module Location = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving typed_fields, sexp, equal, compare]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parser in
    let open Value_parser in
    function
    | X -> from_query_required int
    | Y -> from_query_optional_with_default ~equal:Int.equal int ~default:100
  ;;

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t =
    let open Form.Elements.Textbox in
    function
    | X -> int ()
    | Y -> int ()
  ;;

  let form_of_t : t Form.t Computation.t =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let form_for_field = form_for_field
        let label_for_field = `Inferred
      end)
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.(T [])
end

module Record = struct
  type t =
    { an_int : int
    ; many_floats : float list
    ; optional_string : string option
    ; many_locations : Location.t list
    ; nested : Location.t
    ; username_on_path : string
    ; comment_id_on_path : int
    ; remaining_words_on_path : string list
    }
  [@@deriving typed_fields, sexp, equal, compare]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parser in
    let open Value_parser in
    function
    | An_int -> from_query_required int
    | Many_floats -> from_query_many float
    | Optional_string -> from_query_optional string
    | Many_locations -> from_query_many (sexpable (module Location))
    | Nested -> Parser.Record.make (module Location)
    | Username_on_path -> with_prefix [ "username" ] (from_path string)
    | Comment_id_on_path -> with_prefix [ "id" ] (from_path int)
    | Remaining_words_on_path -> with_prefix [] (from_remaining_path string)
  ;;

  let form_of_t : t Form.t Computation.t =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t =
          let open Form.Elements.Textbox in
          let open Form.Elements in
          function
          | An_int -> int ()
          | Many_floats -> Multiple.list (float ())
          | Optional_string ->
            let%sub form = string () in
            let%arr form = form in
            Form.optional form ~is_some:(fun x -> not (String.equal x "")) ~none:""
          | Many_locations ->
            let location_form = Location.form_of_t in
            Multiple.list location_form
          | Nested -> Location.form_of_t
          | Username_on_path -> string ()
          | Comment_id_on_path -> int ()
          | Remaining_words_on_path -> Multiple.list (string ())
        ;;

        let label_for_field = `Inferred
      end)
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order =
    Path_order.(T [ Username_on_path; Comment_id_on_path; Remaining_words_on_path ])
  ;;
end

module Variant = struct
  type t =
    | Post
    | Comments
  [@@deriving typed_variants, sexp, compare, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Post -> Parser.unit
    | Comments -> Parser.unit
  ;;

  let form_of_t =
    Form.Typed.Variant.make
      (module struct
        module Typed_variant = Typed_variant

        let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
          = function
            | Post -> Bonsai.const (Form.return ())
            | Comments -> Bonsai.const (Form.return ())
        ;;

        let label_for_variant = `Inferred
      end)
  ;;
end

module T = struct
  type t =
    | Homepage
    | Some_string_option of string option
    | Variant of Variant.t
    | Record of Record.t
    | Unable_to_parse
  [@@deriving sexp, equal, compare, typed_variants]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.with_remaining_path [] Parser.unit
    | Some_string_option -> Parser.from_query_optional ~key:"extra" Value_parser.string
    | Variant -> Parser.Variant.make (module Variant)
    | Record -> Parser.Record.make (module Record)
    | Unable_to_parse -> Parser.with_remaining_path [ "unable" ] Parser.unit
  ;;

  let form_of_t =
    Form.Typed.Variant.make
      (module struct
        module Typed_variant = Typed_variant

        let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
          = function
            | Homepage -> Bonsai.const (Form.return ())
            | Some_string_option ->
              let%sub text = Form.Elements.Textbox.string () in
              let%arr text = text in
              Form.optional
                text
                ~is_some:(function
                  | "" -> false
                  | _ -> true)
                ~none:""
            | Variant -> Variant.form_of_t
            | Record -> Record.form_of_t
            | Unable_to_parse -> Bonsai.const (Form.return ())
        ;;

        let label_for_variant = `Inferred
      end)
  ;;
end

let parser = Parser.Variant.make (module T)
let versioned_parser = Url_var.Typed.Versioned_parser.first_parser parser

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /                                                                                        │
    │ /some_string_option                                                                      │
    │ /some_string_option?extra=<string>                                                       │
    │ /unable                                                                                  │
    │ /username/<string>/id/<int>/<multiple<string>>?record.an_int=<int>&record.many_floats=<m │
    │ ultiple<float>>&record.many_locations=<multiple<sexpable>>&record.nested.x=<int>         │
    │ /username/<string>/id/<int>/<multiple<string>>?record.an_int=<int>&record.many_floats=<m │
    │ ultiple<float>>&record.many_locations=<multiple<sexpable>>&record.nested.x=<int>&record. │
    │ nested.y=<int>                                                                           │
    │ /username/<string>/id/<int>/<multiple<string>>?record.an_int=<int>&record.many_floats=<m │
    │ ultiple<float>>&record.many_locations=<multiple<sexpable>>&record.nested.x=<int>&record. │
    │ nested.y=<int>&record.optional_string=<string>                                           │
    │ /username/<string>/id/<int>/<multiple<string>>?record.an_int=<int>&record.many_floats=<m │
    │ ultiple<float>>&record.many_locations=<multiple<sexpable>>&record.nested.x=<int>&record. │
    │ optional_string=<string>                                                                 │
    │ /variant/comments                                                                        │
    │ /variant/post                                                                            │
    └──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let fallback _exn _components = T.Unable_to_parse

let component ~url_var =
  let url_value = Url_var.value url_var in
  let%sub form =
    let%sub form = T.form_of_t in
    let%sub form = Form.Dynamic.with_default url_value form in
    let%sub () =
      Form.Dynamic.on_change
        (module T)
        ~f:(Value.return (fun query -> Url_var.set_effect url_var query))
        form
    in
    return form
  in
  let%arr form = form
  and url = url_value in
  Vdom.Node.div
    [ Vdom.Node.text
        "Change form to update the URL's query! Change the URL's query to update the \
         form! (Errors are detected and routed to the fallback page!)"
    ; Form.view_as_vdom form
    ; Vdom.Node.text "Here's what the parsed query as a sexp looks like:"
    ; Vdom.Node.code [ Vdom.Node.sexp_for_debugging (T.sexp_of_t url) ]
    ]
;;
