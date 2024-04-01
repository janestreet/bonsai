open! Core

module Projection = struct
  type ('a, 'b) t =
    { parse_exn : 'a -> 'b
    ; unparse : 'b -> 'a
    }
  [@@deriving sexp_of]
end

module Percent_encoding_behavior = struct
  type t =
    | Legacy_incorrect
    | Correct
end

module Path_pattern = struct
  type t =
    { pattern : [ `Ignore | `Match of string ] list
    ; needed_match : [ `Prefix | `All ]
    }
  [@@deriving sexp, compare]

  let needed_match_ord = function
    | `All -> 1
    | `Prefix -> 2
  ;;

  (* [x] is strictly more specific than [y] if:
     - [x] has more elements in its pattern than [y]
     - [x] and [y] have the same number of elements, [x] is a total match, and [y] is a
       partial match *)
  let compare_specificity =
    let length_comparison x y =
      let { pattern = x; needed_match = _ } = x
      and { pattern = y; needed_match = _ } = y in
      Int.descending (List.length x) (List.length y)
    in
    let needed_match_comparison x y =
      let { needed_match = x; pattern = _ } = x
      and { needed_match = y; pattern = _ } = y in
      Int.ascending (needed_match_ord x) (needed_match_ord y)
    in
    fun a b -> Comparable.lexicographic [ length_comparison; needed_match_comparison ] a b
  ;;
end

module Value_parser = struct
  type 'a t =
    | String : (string, string) Projection.t -> string t
    | Project :
        { input : 'b t
        ; projection : ('b, 'a) Projection.t
        }
        -> 'a t
    | Int : (string, int) Projection.t -> int t
    | Time_ns :
        (string, Time_ns.Alternate_sexp.t) Projection.t
        -> Time_ns.Alternate_sexp.t t
    | Float : (string, float) Projection.t -> float t
    | Bool : (string, bool) Projection.t -> bool t
    | Stringable : (string, 'a) Projection.t -> 'a t
    | Sexpable : (string, 'a) Projection.t -> 'a t
    | Binable_via_base64 : (string, 'a) Projection.t -> 'a t
    | Base64_encoded : 'a t -> 'a t
    | Fallback :
        { input : 'a t
        ; fallback : 'a
        }
        -> 'a t
    | Name :
        { name : string
        ; input : 'a t
        }
        -> 'a t
  [@@deriving sexp_of]

  let rec eval : type a. a t -> (string, a) Projection.t = function
    | String projection -> projection
    | Int projection -> projection
    | Time_ns projection -> projection
    | Float projection -> projection
    | Bool projection -> projection
    | Stringable projection -> projection
    | Sexpable projection -> projection
    | Binable_via_base64 projection -> projection
    | Base64_encoded input ->
      let input = eval input in
      let decode_exn = Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet in
      let encode = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet in
      { parse_exn = Fn.compose input.parse_exn decode_exn
      ; unparse = Fn.compose encode input.unparse
      }
    | Project { input; projection } ->
      let input = eval input in
      { parse_exn = Fn.compose projection.parse_exn input.parse_exn
      ; unparse = Fn.compose input.unparse projection.unparse
      }
    | Fallback { input; fallback } ->
      let input = eval input in
      { parse_exn =
          (fun x ->
            try input.parse_exn x with
            | _ -> fallback)
      ; unparse = input.unparse
      }
    | Name { name = _; input } -> eval input
  ;;

  module Skeleton = struct
    type 'a parser = 'a t

    type t =
      | String
      | Project of t
      | Int
      | Time_ns
      | Float
      | Bool
      | Stringable
      | Sexpable
      | Binable_via_base64
      | Base64_encoded of t
      | Fallback of t
      | Name of
          { name : string
          ; input : t
          }
    [@@deriving sexp]

    let rec of_parser : type a. a parser -> t = function
      | String _ -> String
      | Project { input; _ } -> Project (of_parser input)
      | Int _ -> Int
      | Time_ns _ -> Time_ns
      | Float _ -> Float
      | Bool _ -> Bool
      | Stringable _ -> Stringable
      | Sexpable _ -> Sexpable
      | Fallback { input; _ } -> Fallback (of_parser input)
      | Binable_via_base64 _ -> Binable_via_base64
      | Base64_encoded input -> Base64_encoded (of_parser input)
      | Name { input; name } -> Name { name; input = of_parser input }
    ;;

    let rec to_summary = function
      | String -> "<string>"
      | Project input -> [%string "<project%{to_summary input}>"]
      | Int -> "<int>"
      | Time_ns -> "<time_ns>"
      | Float -> "<float>"
      | Bool -> "<bool>"
      | Stringable -> "<string>"
      | Sexpable -> "<sexpable>"
      | Fallback input -> [%string "<fallback%{to_summary input}>"]
      | Binable_via_base64 -> "<binable>"
      | Base64_encoded input -> [%string "<base64%{to_summary input}>"]
      | Name { name; _ } -> [%string "<%{name}>"]
    ;;
  end

  let string = String { parse_exn = Fn.id; unparse = Fn.id }

  let project input ~parse_exn ~unparse =
    Project { input; projection = { parse_exn; unparse } }
  ;;

  let int =
    let projection =
      eval (project string ~parse_exn:Int.of_string ~unparse:Int.to_string)
    in
    Int projection
  ;;

  let time_ns =
    let projection =
      eval
        (project
           string
           ~parse_exn:(fun str -> Int63.of_string str |> Time_ns.of_int63_ns_since_epoch)
           ~unparse:(fun ts -> Time_ns.to_int63_ns_since_epoch ts |> Int63.to_string))
    in
    Time_ns projection
  ;;

  let float =
    let projection =
      eval (project string ~parse_exn:Float.of_string ~unparse:Float.to_string)
    in
    Float projection
  ;;

  let bool =
    let projection =
      eval (project string ~parse_exn:Bool.of_string ~unparse:Bool.to_string)
    in
    Bool projection
  ;;

  let stringable (type a) (module M : Stringable with type t = a) =
    let projection = eval (project string ~parse_exn:M.of_string ~unparse:M.to_string) in
    Stringable projection
  ;;

  let sexpable (type a) (module M : Sexpable with type t = a) =
    let projection =
      string
      |> project ~parse_exn:Sexp.of_string ~unparse:Sexp.to_string
      |> project ~parse_exn:M.t_of_sexp ~unparse:M.sexp_of_t
      |> eval
    in
    Sexpable projection
  ;;

  let base64_encoded input = Base64_encoded input

  let binable_via_base64 (type a) (module M : Binable with type t = a) =
    let projection =
      string
      |> project
           ~parse_exn:(Bin_prot.Reader.of_string M.bin_reader_t)
           ~unparse:(Bin_prot.Writer.to_string M.bin_writer_t)
      |> eval
    in
    base64_encoded (Binable_via_base64 projection)
  ;;

  let fallback input ~fallback = Fallback { input; fallback }
  let name name input = Name { name; input }
end

module Components = struct
  type t =
    { path : string list
    ; query : string list String.Map.t
    }
  [@@deriving sexp, equal]

  let empty = { path = []; query = String.Map.empty }

  let encode_path path =
    String.concat ~sep:"/" (List.map ~f:(Uri.pct_encode ~component:`Path) path)
  ;;

  let split_if_nonempty = function
    (* NOTE: We need to special case this situation as we would
       prefer for String.split to return `[]` instead of `[""]` *)
    | "" -> []
    | p -> String.split ~on:'/' p
  ;;

  let decode_path path =
    String.chop_prefix_if_exists ~prefix:"/" path
    |> split_if_nonempty
    |> List.map ~f:Uri.pct_decode
  ;;

  let to_uri ?(encoding_behavior = Percent_encoding_behavior.Correct) { path; query } =
    let path =
      match encoding_behavior with
      | Percent_encoding_behavior.Legacy_incorrect -> String.concat ~sep:"/" path
      | Correct -> encode_path path
    in
    Uri.empty |> Fn.flip Uri.with_path path |> Fn.flip Uri.with_query (Map.to_alist query)
  ;;

  let of_uri ?(encoding_behavior = Percent_encoding_behavior.Correct) uri =
    let path =
      match encoding_behavior with
      | Percent_encoding_behavior.Legacy_incorrect ->
        Uri.path uri |> String.chop_prefix_if_exists ~prefix:"/" |> split_if_nonempty
      | Correct -> decode_path (Uri.path uri)
    in
    let query =
      uri
      |> Uri.query
      |> String.Map.of_alist_multi
      |> Map.filter_map ~f:(function
           | [ value ] -> Some value
           | _ -> None)
    in
    { path; query }
  ;;
end

module Parse_result = struct
  type 'a t =
    { result : 'a
    ; remaining : Components.t
    }
  [@@deriving sexp_of]

  let create a = { result = a; remaining = Components.empty }
end

module type Uri_parser_intf = sig
  type 'a t

  val check_ok_and_print_urls_or_errors : 'a t -> unit

  val eval
    :  ?encoding_behavior:Percent_encoding_behavior.t
    -> 'a t
    -> (Components.t, 'a Parse_result.t) Projection.t

  val eval_for_uri
    :  ?encoding_behavior:Percent_encoding_behavior.t
    -> 'a t
    -> (Uri.t, 'a Parse_result.t) Projection.t

  val all_urls : 'a t -> string list
  val to_string : 'a t -> ('a -> string) Staged.t
end

module Parser = struct
  module rec T : sig
    type 'a t =
      | Unit : unit t
      | Project :
          { input : 'b t
          ; projection : ('b, 'a) Projection.t
          }
          -> 'a t
      | From_query_required :
          { override_key : string option
          ; value_parser : 'a Value_parser.t
          }
          -> 'a t
      | From_query_optional :
          { override_key : string option
          ; value_parser : 'a Value_parser.t
          }
          -> 'a option t
      | From_query_optional_with_default :
          { override_key : string option
          ; equal : 'a -> 'a -> bool
          ; value_parser : 'a Value_parser.t
          ; default : 'a
          }
          -> 'a t
      | From_query_many :
          { override_key : string option
          ; value_parser : 'a Value_parser.t
          }
          -> 'a list t
      | From_path : 'a Value_parser.t -> 'a t
      | From_remaining_path : 'a Value_parser.t -> 'a list t
      | With_prefix :
          { prefix : string list
          ; t : 'a t
          }
          -> 'a t
      | With_remaining_path :
          { needed_path : string list
          ; t : 'a t
          }
          -> 'a t
      | Record :
          { record_module : (module Record.Cached_s with type Typed_field.derived_on = 'a)
          ; override_namespace : string list option
          }
          -> 'a t
      | Variant :
          { variant_module :
              (module Variant.Cached_s with type Typed_variant.derived_on = 'a)
          ; override_namespace : string list option
          }
          -> 'a t
      | Query_based_variant :
          { variant_module :
              (module Query_based_variant.S with type Typed_variant.derived_on = 'a)
          ; override_namespace : string list option
          ; key : string
          }
          -> 'a t
      | Optional_query_fields : { t : 'a t } -> 'a option t
  end =
    T

  and Record : sig
    module Path_order (M : Typed_fields_lib.S) : sig
      type 'a t' =
        | [] : unit t'
        | ( :: ) : 'a M.t * 'b t' -> ('a -> 'b) t'

      type t = T : 'a t' -> t
    end

    module type S = sig
      module Typed_field : Typed_fields_lib.S

      val parser_for_field : 'a Typed_field.t -> 'a T.t
      val path_order : Path_order(Typed_field).t
    end

    module type Cached_s = sig
      module Typed_field : Typed_fields_lib.S

      val parser_for_field : 'a Typed_field.t -> 'a T.t
      val path_order : Typed_field.Packed.t list
    end

    val make
      :  ?namespace:string list
      -> (module S with type Typed_field.derived_on = 'a)
      -> 'a T.t
  end = struct
    module Path_order (M : Typed_fields_lib.S) = struct
      type 'a t' =
        | [] : unit t'
        | ( :: ) : 'a M.t * 'b t' -> ('a -> 'b) t'

      type t = T : 'a t' -> t
    end

    module type S = sig
      module Typed_field : Typed_fields_lib.S

      val parser_for_field : 'a Typed_field.t -> 'a T.t
      val path_order : Path_order(Typed_field).t
    end

    module type Cached_s = sig
      module Typed_field : Typed_fields_lib.S

      val parser_for_field : 'a Typed_field.t -> 'a T.t
      val path_order : Typed_field.Packed.t list
    end

    let make (type a) ?namespace (module M : S with type Typed_field.derived_on = a) =
      let module Parser_map = Typed_field_map.Make_for_records (M.Typed_field) (T) in
      let parser_by_field = Parser_map.create { f = M.parser_for_field } in
      let rec calc_path_order : Path_order(M.Typed_field).t -> M.Typed_field.Packed.t list
        =
        fun path_order ->
        let (T unpacked) = path_order in
        match unpacked with
        | [] -> []
        | hd :: tl -> { M.Typed_field.Packed.f = T hd } :: calc_path_order (T tl)
      in
      let module Cached = struct
        include M

        let parser_for_field : type a. a Typed_field.t -> a T.t =
          fun f -> Parser_map.find parser_by_field f
        ;;

        let path_order = calc_path_order M.path_order
      end
      in
      T.Record { record_module = (module Cached); override_namespace = namespace }
    ;;
  end

  and Variant : sig
    module type S = sig
      module Typed_variant : Typed_variants_lib.S

      val parser_for_variant : 'a Typed_variant.t -> 'a T.t
    end

    module type Cached_s = sig
      include S

      val pattern_for_variant : 'a Typed_variant.t -> Path_pattern.t
    end

    val make
      :  ?namespace:string list
      -> (module S with type Typed_variant.derived_on = 'a)
      -> 'a T.t
  end = struct
    module type S = sig
      module Typed_variant : Typed_variants_lib.S

      val parser_for_variant : 'a Typed_variant.t -> 'a T.t
    end

    module type Cached_s = sig
      include S

      val pattern_for_variant : 'a Typed_variant.t -> Path_pattern.t
    end

    let find_next_declared_path_pattern
      : type a.
        a T.t
        -> [ `Prefix of [ `Ignore | `Match of string ] list
           | `Remaining_path of [ `Ignore | `Match of string ] list
           ]
           option
      =
      fun t ->
      let rec next_declared_path_pattern
        : type a.
          prefix:[ `Ignore | `Match of string ] list
          -> a T.t
          -> ([ `Ignore | `Match of string ] list
             * [ `Continue | `Stop_prefix | `Stop_remaining_path ])
             option
        =
        fun ~prefix t ->
        match t with
        | Unit -> None
        | Project { input; _ } -> next_declared_path_pattern ~prefix input
        | From_query_required _ -> None
        | From_query_optional _ -> None
        | From_query_optional_with_default _ -> None
        | From_query_many _ -> None
        | From_path _ -> Some (prefix @ [ `Ignore ], `Continue)
        | From_remaining_path _ -> Some (prefix, `Continue)
        | With_prefix { prefix = inner_prefix; _ } ->
          Some (prefix @ List.map inner_prefix ~f:(fun x -> `Match x), `Stop_prefix)
        | With_remaining_path { needed_path; _ } ->
          Some (prefix @ List.map needed_path ~f:(fun x -> `Match x), `Stop_remaining_path)
        | Record { record_module; _ } ->
          let module M =
            (val record_module : Record.Cached_s with type Typed_field.derived_on = a)
          in
          List.fold M.path_order ~init:None ~f:(fun acc { f = T f } ->
            match acc with
            | Some (_, `Stop_prefix) | Some (_, `Stop_remaining_path) -> acc
            | None -> next_declared_path_pattern ~prefix (M.parser_for_field f)
            | Some (prefix, `Continue) ->
              next_declared_path_pattern ~prefix (M.parser_for_field f))
        | Variant _ -> None
        | Query_based_variant _ -> None
        | Optional_query_fields { t } -> next_declared_path_pattern ~prefix t
      in
      match next_declared_path_pattern ~prefix:[] t with
      | None -> None
      | Some (_, `Continue) -> None
      | Some (x, `Stop_prefix) -> Some (`Prefix x)
      | Some (x, `Stop_remaining_path) -> Some (`Remaining_path x)
    ;;

    let make (type a) ?namespace (module M : S with type Typed_variant.derived_on = a) =
      let module Parser_map = Typed_field_map.Make (M.Typed_variant) (T) in
      let parser_by_variant = Parser_map.create { f = M.parser_for_variant } in
      let module Pattern_map =
        Typed_field_map.Make
          (M.Typed_variant)
          (struct
            type _ t = Path_pattern.t * bool
          end)
      in
      let create_pattern_map_for_variant v =
        let parser = M.parser_for_variant v in
        match find_next_declared_path_pattern parser with
        | None ->
          ( { Path_pattern.pattern = [ `Match (M.Typed_variant.name v) ]
            ; needed_match = `Prefix
            }
          , true )
        | Some (`Prefix pattern) ->
          { Path_pattern.pattern; needed_match = `Prefix }, false
        | Some (`Remaining_path pattern) ->
          { Path_pattern.pattern; needed_match = `All }, false
      in
      let patterns_by_variant =
        Pattern_map.create { f = create_pattern_map_for_variant }
      in
      let pattern_for_variant v = Pattern_map.find patterns_by_variant v in
      let module Cached = struct
        include M

        let parser_for_variant : type a. a Typed_variant.t -> a T.t =
          fun f ->
          let out = Parser_map.find parser_by_variant f in
          match Tuple2.get2 (pattern_for_variant f) with
          | false -> out
          | true ->
            T.With_prefix
              { prefix =
                  (Tuple2.get1 (pattern_for_variant f)).pattern
                  |> List.filter_map ~f:(function
                       | `Match x -> Some x
                       | _ -> None)
              ; t = out
              }
        ;;

        let pattern_for_variant : type a. a Typed_variant.t -> Path_pattern.t =
          fun v ->
          let pattern, _ = pattern_for_variant v in
          pattern
        ;;
      end
      in
      T.Variant { variant_module = (module Cached); override_namespace = namespace }
    ;;
  end

  and Query_based_variant : sig
    module type S = sig
      module Typed_variant : Typed_variants_lib.S

      val parser_for_variant : 'a Typed_variant.t -> 'a T.t
      val identifier_for_variant : 'a Typed_variant.t -> string
    end

    val make
      :  ?namespace:string list
      -> (module S with type Typed_variant.derived_on = 'a)
      -> key:string
      -> 'a T.t
  end = struct
    module type S = sig
      module Typed_variant : Typed_variants_lib.S

      val parser_for_variant : 'a Typed_variant.t -> 'a T.t
      val identifier_for_variant : 'a Typed_variant.t -> string
    end

    let make
      (type a)
      ?namespace
      (module M : S with type Typed_variant.derived_on = a)
      ~key
      =
      let module Parser_map = Typed_field_map.Make (M.Typed_variant) (T) in
      let parsers_by_variant = Parser_map.create { f = M.parser_for_variant } in
      let module Identifier_map =
        Typed_field_map.Make
          (M.Typed_variant)
          (struct
            type _ t = string
          end)
      in
      let identifiers_by_variant =
        Identifier_map.create { f = M.identifier_for_variant }
      in
      let module Cached = struct
        include M

        let parser_for_variant v = Parser_map.find parsers_by_variant v
        let identifier_for_variant v = Identifier_map.find identifiers_by_variant v
      end
      in
      T.Query_based_variant
        { variant_module = (module Cached); override_namespace = namespace; key }
    ;;
  end

  type 'a t = 'a T.t

  let unit = T.Unit

  let from_query_required ?key value_parser =
    T.From_query_required { override_key = key; value_parser }
  ;;

  let from_query_optional ?key value_parser =
    T.From_query_optional { override_key = key; value_parser }
  ;;

  let from_query_optional_with_default ?key ~equal value_parser ~default =
    T.From_query_optional_with_default
      { equal; override_key = key; value_parser; default }
  ;;

  let project input ~parse_exn ~unparse =
    T.Project { input; projection = { parse_exn; unparse } }
  ;;

  let from_query_many ?key value_parser =
    T.From_query_many { override_key = key; value_parser }
  ;;

  let from_query_many_at_least_1 ?key value_parser =
    project
      (from_query_many ?key value_parser)
      ~parse_exn:(fun values ->
        if List.is_empty values
        then
          raise_s
            [%message
              "Error! [from_query_many_at_least_1] expected at least one element in the \
               list."];
        values)
      ~unparse:Fn.id
  ;;

  let from_path value_parser = T.From_path value_parser
  let from_remaining_path value_parser = T.From_remaining_path value_parser
  let with_prefix prefix t = T.With_prefix { prefix; t }
  let with_remaining_path needed_path t = T.With_remaining_path { needed_path; t }
  let end_of_path t = T.With_remaining_path { needed_path = []; t }
  let optional_query_fields input = T.Optional_query_fields { t = input }

  let rec needs_to_appear_on_path_order : type a. a T.t -> bool = function
    | Project { input; _ } -> needs_to_appear_on_path_order input
    | Optional_query_fields { t } -> needs_to_appear_on_path_order t
    | Record { record_module; _ } ->
      let module M =
        (val record_module : Record.Cached_s with type Typed_field.derived_on = a)
      in
      List.exists M.Typed_field.Packed.all ~f:(fun { f = T f } ->
        needs_to_appear_on_path_order (M.parser_for_field f))
    | Variant { variant_module; _ } ->
      let module M =
        (val variant_module : Variant.Cached_s with type Typed_variant.derived_on = a)
      in
      List.exists M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
        needs_to_appear_on_path_order (M.parser_for_variant v))
    | Query_based_variant { variant_module; _ } ->
      let module M =
        (val variant_module
            : Query_based_variant.S with type Typed_variant.derived_on = a)
      in
      List.exists M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
        needs_to_appear_on_path_order (M.parser_for_variant v))
    | With_remaining_path _ | With_prefix _ | From_path _ | From_remaining_path _ -> true
    | Unit
    | From_query_required _
    | From_query_optional _
    | From_query_optional_with_default _
    | From_query_many _ -> false
  ;;

  exception Missing_key of string

  module Eval = struct
    let raise_can't_infer_key_error () =
      raise_s
        [%message
          "Could not infer query key! In most cases, the keys of the key of a query \
           field can be inferred from the labels of the record it's in or the \
           constructor of the variant it's in. However, the [from_query_*] parser's key \
           name could not be inferred. You can fix this in two ways. 1. Wrap your type \
           around a record/variant OR 2. explicitly assign a key through the optional \
           [?key] parameter."]
    ;;

    let read_query_key ~override_key ~inferred_name_from_parent ~current_namespace ~f =
      match Option.first_some override_key inferred_name_from_parent with
      | Some k -> f (String.concat ~sep:"." (current_namespace @ [ k ]))
      | None -> raise_can't_infer_key_error ()
    ;;

    let if_field_is_present_in_query ~query ~key_name ~f =
      match Map.find query key_name with
      | None -> raise (Missing_key key_name)
      | Some value -> f value
    ;;

    let raise_if_empty values =
      if List.is_empty values
      then raise_s [%message "Expected a value in query field, but nothing was present"]
    ;;

    let raise_if_empty_path values =
      if List.is_empty values
      then raise_s [%message "Expected a value in path, but nothing was present"]
    ;;

    let namespace_for_record_field
      ~current_namespace
      ~override_namespace
      ~parent_namespace
      =
      current_namespace @ Option.value override_namespace ~default:parent_namespace
    ;;

    let eval_from_query_required
      (type a)
      ~override_key
      ~(value_parser : a Value_parser.t)
      ~current_namespace
      ~inferred_name_from_parent
      =
      let value_projection = Value_parser.eval value_parser in
      let parse_exn (components : Components.t) =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let value_to_parse, remaining =
            if_field_is_present_in_query
              ~query:components.query
              ~key_name
              ~f:(fun values ->
              raise_if_empty values;
              let remaining_query = Map.remove components.query key_name in
              let remaining = { components with query = remaining_query } in
              List.hd_exn values, remaining)
          in
          let result = value_projection.parse_exn value_to_parse in
          { Parse_result.result; remaining })
      in
      let unparse (result : a Parse_result.t) =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let query =
            let unparse_result = value_projection.unparse result.result in
            Map.set result.remaining.query ~key:key_name ~data:[ unparse_result ]
          in
          { result.remaining with query })
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_from_query_optional
      (type a)
      ~override_key
      ~(value_parser : a Value_parser.t)
      ~current_namespace
      ~inferred_name_from_parent
      =
      let value_projection = Value_parser.eval value_parser in
      let parse_exn (components : Components.t) =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let result, remaining =
            match Map.find components.query key_name with
            | None -> None, components
            | Some values ->
              raise_if_empty values;
              let result =
                let value_to_parse = List.hd_exn values in
                Some (value_projection.parse_exn value_to_parse)
              in
              let remaining =
                let query = Map.remove components.query key_name in
                { components with query }
              in
              result, remaining
          in
          { Parse_result.result; remaining })
      in
      let unparse { Parse_result.result; remaining = { path = _; query } as remaining } =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let query =
            match result with
            | None -> query
            | Some value ->
              Map.set query ~key:key_name ~data:[ value_projection.unparse value ]
          in
          { remaining with query })
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_from_query_optional_with_default
      (type a)
      ~override_key
      ~(value_parser : a Value_parser.t)
      ~equal
      ~default
      ~current_namespace
      ~inferred_name_from_parent
      =
      let value_projection = Value_parser.eval value_parser in
      let parse_exn (components : Components.t) =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let result, remaining =
            match Map.find components.query key_name with
            | None -> default, components
            | Some values ->
              raise_if_empty values;
              let result =
                let value_to_parse = List.hd_exn values in
                value_projection.parse_exn value_to_parse
              in
              let remaining =
                let query = Map.remove components.query key_name in
                { components with query }
              in
              result, remaining
          in
          { Parse_result.result; remaining })
      in
      let unparse { Parse_result.result; remaining = { path = _; query } as remaining } =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let query =
            (* If the value that needs to be unparsed happens to be "equal" (as defined
                 by the user) to the default value, then then value is not included in the
                 prefix. *)
            match equal result default with
            | true -> query
            | false ->
              let unparse_result = value_projection.unparse result in
              Map.set query ~key:key_name ~data:[ unparse_result ]
          in
          { remaining with query })
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_from_query_many
      (type a)
      ~override_key
      ~(value_parser : a Value_parser.t)
      ~current_namespace
      ~inferred_name_from_parent
      =
      let value_projection = Value_parser.eval value_parser in
      let parse_exn (components : Components.t) =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let result, remaining_query =
            match Map.find components.query key_name with
            | None -> [], components.query
            | Some values ->
              ( List.map values ~f:value_projection.parse_exn
              , Map.remove components.query key_name )
          in
          let remaining = { components with query = remaining_query } in
          { Parse_result.result; remaining })
      in
      let unparse { Parse_result.result; remaining } =
        read_query_key
          ~override_key
          ~inferred_name_from_parent
          ~current_namespace
          ~f:(fun key_name ->
          let query =
            match result with
            | [] -> remaining.query
            | _ :: _ ->
              Map.set
                remaining.query
                ~key:key_name
                ~data:(List.map result ~f:value_projection.unparse)
          in
          { remaining with query })
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_from_path (type a) ~(value_parser : a Value_parser.t) =
      let value_projection = Value_parser.eval value_parser in
      let parse_exn (components : Components.t) =
        let result, remaining_path =
          let path = components.path in
          raise_if_empty_path path;
          value_projection.parse_exn (List.hd_exn path), List.tl_exn path
        in
        let remaining = { components with path = remaining_path } in
        { Parse_result.result; remaining }
      in
      let unparse { Parse_result.result; remaining } =
        let unparsed_value = value_projection.unparse result in
        let path = unparsed_value :: remaining.path in
        { remaining with path }
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_from_remaining_path (type a) ~(value_parser : a Value_parser.t) =
      let value_projection = Value_parser.eval value_parser in
      let parse_exn (components : Components.t) =
        let result = List.map components.path ~f:value_projection.parse_exn in
        let remaining = { components with path = [] } in
        { Parse_result.result; remaining }
      in
      let unparse { Parse_result.result; remaining } =
        let unparsed_values = List.map result ~f:value_projection.unparse in
        let path = unparsed_values @ remaining.path in
        { remaining with path }
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_project
      (type a b)
      ~(input : (Components.t, a Parse_result.t) Projection.t)
      ~(projection : (a, b) Projection.t)
      =
      let parse_exn (components : Components.t) =
        let intermediate_result = input.parse_exn components in
        { Parse_result.result = projection.parse_exn intermediate_result.result
        ; remaining = intermediate_result.remaining
        }
      in
      let unparse (result : b Parse_result.t) =
        let intermediate_result = projection.unparse result.result in
        input.unparse
          { Parse_result.result = intermediate_result; remaining = result.remaining }
      in
      { Projection.parse_exn; unparse }
    ;;

    let remaining_after_prefix ~prefix ~path =
      if List.is_prefix path ~prefix ~equal:String.equal
      then Some (List.drop path (List.length prefix))
      else None
    ;;

    let eval_with_prefix
      (type a)
      (t : (Components.t, a Parse_result.t) Projection.t)
      ~prefix
      =
      let parse_exn (components : Components.t) =
        let remaining_path = remaining_after_prefix ~prefix ~path:components.path in
        match remaining_path with
        | None ->
          raise_s
            [%message
              "Did not recognize url during parsing! Expected path to match a \
               [with_prefix] prefix, but path did not match prefix!"
                (prefix : string list)
                (components : Components.t)]
        | Some remaining_path -> t.parse_exn { components with path = remaining_path }
      in
      let unparse (result : a Parse_result.t) =
        let components = t.unparse result in
        { components with path = prefix @ components.path }
      in
      { Projection.parse_exn; unparse }
    ;;

    let eval_with_remaining_path
      (type a)
      (t : (Components.t, a Parse_result.t) Projection.t)
      ~needed_path
      =
      let parse_exn (components : Components.t) =
        match List.equal String.equal needed_path components.path with
        | false ->
          raise_s
            [%message
              "Did not recognize url during parsing! Expected path to match an expected \
               [with_remaining_path] needed path! but\n\
              \              the needed path was not recognized!"
                (needed_path : string list)
                (components : Components.t)]
        | true -> t.parse_exn { components with path = [] }
      in
      let unparse (result : a Parse_result.t) =
        let components = t.unparse result in
        { components with path = needed_path @ components.path }
      in
      { Projection.parse_exn; unparse }
    ;;

    let raise_if_path_order_has_duplicates
      (type a)
      (module M : Record.Cached_s with type Typed_field.derived_on = a)
      : unit
      =
      try
        List.exn_if_dup
          M.path_order
          ~compare:M.Typed_field.Packed.compare
          ~to_sexp:M.Typed_field.Packed.sexp_of_t
      with
      | _ -> raise_s [%message "Path order cannot have duplicates!"]
    ;;

    let raise_if_path_field_does_not_have_order
      (type a)
      (module M : Record.Cached_s with type Typed_field.derived_on = a)
      : unit
      =
      let module Packed_typed_field = struct
        include M.Typed_field.Packed
        include Comparable.Make (M.Typed_field.Packed)
      end
      in
      let fields_that_need_path =
        List.filter M.Typed_field.Packed.all ~f:(fun { f = T f } ->
          let parser = M.parser_for_field f in
          needs_to_appear_on_path_order parser)
        |> Packed_typed_field.Set.of_list
      in
      let fields_with_path = Packed_typed_field.Set.of_list M.path_order in
      let missing_fields = Set.diff fields_that_need_path fields_with_path in
      if not (Set.is_empty missing_fields)
      then
        raise_s
          [%message
            "Each path parser must be present in path order. The following fields were \
             missing:"
              (missing_fields : Packed_typed_field.Set.t)]
    ;;

    let rec eval_record
      : type a.
        (module Record.Cached_s with type Typed_field.derived_on = a)
        -> override_namespace:string list option
        -> more_path_parsing_allowed:bool
        -> current_namespace:string list
        -> parent_namespace:string list
        -> (Components.t, a Parse_result.t) Projection.t
      =
      fun (module M : Record.Cached_s with type Typed_field.derived_on = a)
          ~(override_namespace : string list option)
          ~more_path_parsing_allowed
          ~current_namespace
          ~parent_namespace ->
      (* Caching evaluated projections. *)
      let module Projection_map =
        Typed_field_map.Make_for_records
          (M.Typed_field)
          (struct
            type 'a t = (Components.t, 'a Parse_result.t) Projection.t
          end)
      in
      let eval_field f =
        let inferred_field_name = M.Typed_field.name f in
        eval
          ~more_path_parsing_allowed
          ~current_namespace:
            (namespace_for_record_field
               ~current_namespace
               ~override_namespace
               ~parent_namespace)
          ~inferred_name_from_parent:(Some inferred_field_name)
          ~parent_namespace:[ inferred_field_name ]
          (M.parser_for_field f)
      in
      let projections_by_field = Projection_map.create { f = eval_field } in
      let projection_for_field f = Projection_map.find projections_by_field f in
      (* Unfortunately, the parsed record can't be created in one go with [create] since
           path parsing cares about the order that the path fields are found in
           [path_order]. A [Typed_field_map] with an [Option] data is used to set the
           parsed values in the desired order.

           Here is the order for [parse_exn]:

           1. Call [parse_exn] for fields that appear in [path_order] in the order they
           appear in [path_order] and store their results in the result map.
           2. Call [parse_exn] for all other fields in any order, and also store their
           results in the result map.
           3. At this point every value in the map is [Some], so now [create] can be
           called to produce the record in one go.

           Here is the order for [unparse] (it's the inverse of [parse_exn]):

           1. Call [unparse] for all fields except those that are in the path (folding
           over the remaining components)
           2. Call [unparse] for the fields that appear in the path in the inverse order
           (folding over the remaining components).
           3. Unparsed result is in remaining functions.
        *)
      let module Result_map = Typed_field_map.Make (M.Typed_field) (Option) in
      (* [parse_order] has the path order fields at the beginning and all of the
           remaining fields at the end to satisfy order dependent steps trivially. *)
      let parse_order =
        let module Packed_field = struct
          module T = struct
            include M.Typed_field.Packed
          end

          include T
          include Comparable.Make (T)
        end
        in
        let path_order_set = Packed_field.Set.of_list M.path_order in
        let fields_not_in_path =
          List.filter M.Typed_field.Packed.all ~f:(fun f ->
            not (Set.mem path_order_set f))
        in
        M.path_order @ fields_not_in_path
      in
      let parse_exn (components : Components.t) =
        raise_if_path_order_has_duplicates (module M);
        raise_if_path_field_does_not_have_order (module M);
        let empty_results =
          { Parse_result.result = Result_map.create { f = (fun _ -> None) }
          ; remaining = components
          }
        in
        let results =
          List.fold parse_order ~init:empty_results ~f:(fun results { f = T f } ->
            let projection = projection_for_field f in
            let { Parse_result.result; remaining } =
              try projection.parse_exn results.remaining with
              | Missing_key x -> raise (Missing_key x)
              | e ->
                let error_message = Exn.sexp_of_t e in
                let field_name = M.Typed_field.name f in
                let unparseable_components = results.remaining in
                raise_s
                  [%message
                    "Error while parsing record field:"
                      (error_message : Sexp.t)
                      (field_name : string)
                      (unparseable_components : Components.t)]
            in
            let result = Result_map.set results.result ~key:f ~data:(Some result) in
            { Parse_result.result; remaining })
        in
        let result =
          M.Typed_field.create
            { f =
                (fun f ->
                  Option.value_or_thunk
                    (Result_map.find results.result f)
                    ~default:(fun () ->
                    raise_s
                      [%message
                        "Internal Bug: Result for a record field was never parsed"
                          ({ f = T f } : M.Typed_field.Packed.t)]))
            }
        in
        { Parse_result.result; remaining = results.remaining }
      in
      let unparse (result : a Parse_result.t) =
        List.fold
          (List.rev parse_order)
          ~init:result.remaining
          ~f:(fun components { f = T f } ->
          let projection = projection_for_field f in
          let result = M.Typed_field.get f result.result in
          projection.unparse { Parse_result.result; remaining = components })
      in
      { Projection.parse_exn; unparse }

    and eval_variant
      : type a.
        (module Variant.Cached_s with type Typed_variant.derived_on = a)
        -> override_namespace:string list option
        -> more_path_parsing_allowed:bool
        -> current_namespace:string list
        -> parent_namespace:string list
        -> (Components.t, a Parse_result.t) Projection.t
      =
      fun (module M : Variant.Cached_s with type Typed_variant.derived_on = a)
          ~(override_namespace : string list option)
          ~more_path_parsing_allowed
          ~current_namespace
          ~parent_namespace ->
      (* Caching evaluated projections *)
      let module Projection_map =
        Typed_field_map.Make
          (M.Typed_variant)
          (struct
            type 'a t = (Components.t, 'a Parse_result.t) Projection.t
          end)
      in
      let eval_constructor v =
        let inferred_constructor_name = M.Typed_variant.name v in
        eval
          ~more_path_parsing_allowed
          ~current_namespace:
            (namespace_for_record_field
               ~current_namespace
               ~override_namespace
               ~parent_namespace)
          ~inferred_name_from_parent:(Some inferred_constructor_name)
          ~parent_namespace:[ inferred_constructor_name ]
          (M.parser_for_variant v)
      in
      let projections_by_variant = Projection_map.create { f = eval_constructor } in
      let projection_for_variant v = Projection_map.find projections_by_variant v in
      (* Caching which pattern is expected for each variant. *)
      let rec path_matches_pattern ~(pattern : Path_pattern.t) ~path =
        match pattern.pattern, path with
        | [], _ ->
          (match pattern.needed_match with
           | `All -> List.is_empty path
           | `Prefix -> true)
        | `Ignore :: pattern_tl, _ :: path_tl ->
          path_matches_pattern
            ~pattern:{ pattern with pattern = pattern_tl }
            ~path:path_tl
        | `Match pattern_hd :: pattern_tl, path_hd :: path_tl ->
          String.equal pattern_hd path_hd
          && path_matches_pattern
               ~pattern:{ pattern with pattern = pattern_tl }
               ~path:path_tl
        | _ -> false
      in
      (* For variants, the parse order is "most-specific to least-specific". This is
           defined as:
           1. Which variant's path pattern has more elements
           2. (If tied) total matches come before partial matches *)
      let parse_order =
        List.sort M.Typed_variant.Packed.all ~compare:(fun { f = T va } { f = T vb } ->
          Path_pattern.compare_specificity
            (M.pattern_for_variant va)
            (M.pattern_for_variant vb))
      in
      let parse_exn (components : Components.t) =
        let result =
          List.find_map parse_order ~f:(fun { f = T v } ->
            if path_matches_pattern
                 ~pattern:(M.pattern_for_variant v)
                 ~path:components.path
            then (
              let projection = projection_for_variant v in
              let parse_result = projection.parse_exn components in
              let variant = M.Typed_variant.create v parse_result.result in
              Some { Parse_result.result = variant; remaining = parse_result.remaining })
            else None)
        in
        Option.value_or_thunk result ~default:(fun () ->
          let available_patterns =
            List.map M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
              M.Typed_variant.name v, M.pattern_for_variant v)
          in
          raise_s
            [%message
              "Error while parsing! No matching variant contructor found for current \
               path!"
                (components : Components.t)
                (available_patterns : (string * Path_pattern.t) list)])
      in
      let unparse (result : a Parse_result.t) =
        let { f = T v } = M.Typed_variant.which result.result in
        let inner_result = M.Typed_variant.get v result.result |> Option.value_exn in
        let projection = projection_for_variant v in
        projection.unparse
          { Parse_result.result = inner_result; remaining = result.remaining }
      in
      { Projection.parse_exn; unparse }

    and eval_query_based_variant
      : type a.
        (module Query_based_variant.S with type Typed_variant.derived_on = a)
        -> override_namespace:string list option
        -> more_path_parsing_allowed:bool
        -> current_namespace:string list
        -> parent_namespace:string list
        -> key:string
        -> (Components.t, a Parse_result.t) Projection.t
      =
      fun (module M)
          ~override_namespace
          ~more_path_parsing_allowed
          ~current_namespace
          ~parent_namespace
          ~key ->
      (* Caching evaluated projections *)
      let module Projection_map =
        Typed_field_map.Make
          (M.Typed_variant)
          (struct
            type 'a t = (Components.t, 'a Parse_result.t) Projection.t
          end)
      in
      let eval_constructor v =
        let inferred_constructor_name = M.Typed_variant.name v in
        eval
          ~more_path_parsing_allowed
          ~current_namespace:
            (namespace_for_record_field
               ~current_namespace
               ~override_namespace
               ~parent_namespace)
          ~inferred_name_from_parent:(Some inferred_constructor_name)
          ~parent_namespace:[ inferred_constructor_name ]
          (M.parser_for_variant v)
      in
      let projections_by_variant = Projection_map.create { f = eval_constructor } in
      let projection_for_variant v = Projection_map.find projections_by_variant v in
      let variant_by_identifier =
        (* Fails at eval_time if two different variants expect the same value. *)
        List.fold M.Typed_variant.Packed.all ~init:String.Map.empty ~f:(fun acc variant ->
          let key =
            let { f = T v } = variant in
            M.identifier_for_variant v
          in
          match Map.add acc ~key ~data:variant with
          | `Duplicate -> raise_s [%message "found duplicate identifier!" key]
          | `Ok acc -> acc)
      in
      let parse_exn (components : Components.t) =
        match Map.find components.query key with
        | None | Some [] ->
          raise_s
            [%message
              [%string
                {|Error while parsing url! Expected key "%{key}=<page>" inside of the url's query.|}]]
        | Some (_ :: _ :: _) ->
          raise_s
            [%message
              [%string
                {|Error while parsing! Got multiple "%{key}" query parameters, but expected just one.|}]]
        | Some [ single_identifier ] ->
          (match Map.find variant_by_identifier single_identifier with
           | None ->
             let expected_one_of =
               List.map M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
                 M.Typed_variant.name v, M.identifier_for_variant v)
             in
             raise_s
               [%message
                 [%string
                   {|Error while parsing! Got unexpected value "%{single_identifier}" for "%{key}" query parameter.|}]
                   (expected_one_of : (string * string) list)]
           | Some { f = T v } ->
             let components =
               { components with query = Map.remove components.query key }
             in
             let projection = projection_for_variant v in
             let parse_result = projection.parse_exn components in
             let variant = M.Typed_variant.create v parse_result.result in
             { Parse_result.result = variant; remaining = parse_result.remaining })
      in
      let unparse (result : a Parse_result.t) =
        let { f = T v } = M.Typed_variant.which result.result in
        let inner_result = M.Typed_variant.get v result.result |> Option.value_exn in
        let projection = projection_for_variant v in
        let query =
          match
            Map.add result.remaining.query ~key ~data:[ M.identifier_for_variant v ]
          with
          | `Ok query -> query
          | `Duplicate ->
            raise_s
              [%message
                "query key was added multiple times while unparsing a URL. This likely \
                 indicates an illegal URL structure, which will be caught if you run \
                 [check_ok_and_print_urls_or_errors]. If that function doesn't complain, \
                 this is a bug."]
        in
        let remaining = { result.remaining with query } in
        projection.unparse { Parse_result.result = inner_result; remaining }
      in
      { Projection.parse_exn; unparse }

    and eval_optional_query_fields
      : type a.
        (Components.t, a Parse_result.t) Projection.t
        -> (Components.t, a option Parse_result.t) Projection.t
      =
      fun t ->
      let parse_exn (components : Components.t) =
        try
          let result = t.parse_exn components in
          { Parse_result.result = Some result.result; remaining = result.remaining }
        with
        | Missing_key _ -> { Parse_result.result = None; remaining = components }
      in
      let unparse (parse_result : a option Parse_result.t) =
        match parse_result.result with
        | None -> parse_result.remaining
        | Some result ->
          t.unparse { Parse_result.result; remaining = parse_result.remaining }
      in
      { Projection.parse_exn; unparse }

    and eval
      : type a.
        a T.t
        -> more_path_parsing_allowed:bool
        -> current_namespace:string list
        -> inferred_name_from_parent:string option
        -> parent_namespace:string list
        -> (Components.t, a Parse_result.t) Projection.t
      =
      fun t
          ~more_path_parsing_allowed
          ~current_namespace
          ~inferred_name_from_parent
          ~parent_namespace ->
      match t with
      | Unit ->
        { Projection.parse_exn =
            (fun components -> { Parse_result.result = (); remaining = components })
        ; unparse = (fun result -> result.remaining)
        }
      | Project { input; projection } ->
        let input =
          eval
            ~more_path_parsing_allowed
            ~current_namespace
            ~inferred_name_from_parent
            ~parent_namespace
            input
        in
        eval_project ~input ~projection
      | From_query_required { override_key; value_parser } ->
        eval_from_query_required
          ~override_key
          ~value_parser
          ~current_namespace
          ~inferred_name_from_parent
      | From_query_optional { override_key; value_parser } ->
        eval_from_query_optional
          ~override_key
          ~value_parser
          ~current_namespace
          ~inferred_name_from_parent
      | From_query_optional_with_default { override_key; equal; value_parser; default } ->
        eval_from_query_optional_with_default
          ~override_key
          ~value_parser
          ~equal
          ~default
          ~current_namespace
          ~inferred_name_from_parent
      | From_query_many { override_key; value_parser } ->
        eval_from_query_many
          ~override_key
          ~value_parser
          ~current_namespace
          ~inferred_name_from_parent
      | From_path value_parser -> eval_from_path ~value_parser
      | From_remaining_path value_parser -> eval_from_remaining_path ~value_parser
      | With_prefix { prefix; t } ->
        let t =
          eval
            ~more_path_parsing_allowed
            ~current_namespace
            ~inferred_name_from_parent
            ~parent_namespace
            t
        in
        eval_with_prefix t ~prefix
      | With_remaining_path { needed_path; t } ->
        let t =
          eval
            ~more_path_parsing_allowed:false
            ~current_namespace
            ~inferred_name_from_parent
            ~parent_namespace
            t
        in
        eval_with_remaining_path t ~needed_path
      | Record { record_module; override_namespace } ->
        eval_record
          record_module
          ~override_namespace
          ~more_path_parsing_allowed
          ~current_namespace
          ~parent_namespace
      | Variant { variant_module; override_namespace } ->
        eval_variant
          variant_module
          ~override_namespace
          ~more_path_parsing_allowed
          ~current_namespace
          ~parent_namespace
      | Query_based_variant { variant_module; override_namespace; key } ->
        eval_query_based_variant
          variant_module
          ~override_namespace
          ~more_path_parsing_allowed
          ~current_namespace
          ~parent_namespace
          ~key
      | Optional_query_fields { t } ->
        let t =
          eval
            t
            ~more_path_parsing_allowed
            ~current_namespace
            ~inferred_name_from_parent
            ~parent_namespace
        in
        eval_optional_query_fields t
    ;;
  end

  module Skeleton = struct
    type t =
      | Unit
      | Project of t
      | From_query_required of
          { override_key : string option [@sexp.option]
          ; value_parser : Value_parser.Skeleton.t
          }
      | From_query_optional of
          { override_key : string option [@sexp.option]
          ; value_parser : Value_parser.Skeleton.t
          }
      | From_query_optional_with_default of
          { override_key : string option [@sexp.option]
          ; value_parser : Value_parser.Skeleton.t
          }
      | From_query_many of
          { override_key : string option [@sexp.option]
          ; value_parser : Value_parser.Skeleton.t
          }
      | From_path of Value_parser.Skeleton.t
      | From_remaining_path of Value_parser.Skeleton.t
      | With_prefix of
          { prefix : string list
          ; t : t
          }
      | With_remaining_path of
          { needed_path : string list
          ; t : t
          }
      | Record of
          { override_namespace : string list option [@sexp.option]
          ; label_declarations : t String.Map.t
          ; path_order : string list
          }
      | Variant of
          { constructor_declarations : t String.Map.t
          ; patterns : Path_pattern.t String.Map.t
          ; override_namespace : string list option
          }
      | Query_based_variant of
          { constructor_declarations : t String.Map.t
          ; identifiers : string String.Map.t
          ; override_namespace : string list option
          ; key : string
          }
      | Optional_query_fields of { t : t }
    [@@deriving sexp]

    let rec of_parser : type a. a T.t -> t = function
      | Unit -> Unit
      | Project { input; _ } -> Project (of_parser input)
      | From_query_required { override_key; value_parser } ->
        From_query_required
          { override_key; value_parser = Value_parser.Skeleton.of_parser value_parser }
      | From_query_optional { override_key; value_parser } ->
        From_query_optional
          { override_key; value_parser = Value_parser.Skeleton.of_parser value_parser }
      | From_query_optional_with_default { override_key; value_parser; _ } ->
        From_query_optional_with_default
          { override_key; value_parser = Value_parser.Skeleton.of_parser value_parser }
      | From_query_many { override_key; value_parser } ->
        From_query_many
          { override_key; value_parser = Value_parser.Skeleton.of_parser value_parser }
      | From_path value_parser -> From_path (Value_parser.Skeleton.of_parser value_parser)
      | From_remaining_path value_parser ->
        From_remaining_path (Value_parser.Skeleton.of_parser value_parser)
      | With_prefix { prefix; t } -> With_prefix { prefix; t = of_parser t }
      | With_remaining_path { needed_path; t } ->
        With_remaining_path { needed_path; t = of_parser t }
      | Record { record_module; override_namespace } ->
        let module M =
          (val record_module : Record.Cached_s with type Typed_field.derived_on = a)
        in
        let label_declarations =
          List.fold
            M.Typed_field.Packed.all
            ~init:String.Map.empty
            ~f:(fun acc { f = T f } ->
            Map.set
              acc
              ~key:(M.Typed_field.name f)
              ~data:(of_parser (M.parser_for_field f)))
        in
        let path_order =
          List.map M.path_order ~f:(fun { f = T f } -> M.Typed_field.name f)
        in
        Record { override_namespace; label_declarations; path_order }
      | Variant { variant_module; override_namespace } ->
        let module M =
          (val variant_module : Variant.Cached_s with type Typed_variant.derived_on = a)
        in
        let constructor_declarations =
          List.fold
            M.Typed_variant.Packed.all
            ~init:String.Map.empty
            ~f:(fun acc { f = T v } ->
            Map.set
              acc
              ~key:(M.Typed_variant.name v)
              ~data:(of_parser (M.parser_for_variant v)))
        in
        let patterns =
          List.fold
            M.Typed_variant.Packed.all
            ~init:String.Map.empty
            ~f:(fun acc { f = T v } ->
            Map.set acc ~key:(M.Typed_variant.name v) ~data:(M.pattern_for_variant v))
        in
        Variant { override_namespace; constructor_declarations; patterns }
      | Query_based_variant { variant_module; override_namespace; key } ->
        let module M =
          (val variant_module
              : Query_based_variant.S with type Typed_variant.derived_on = a)
        in
        let constructor_declarations =
          List.fold
            M.Typed_variant.Packed.all
            ~init:String.Map.empty
            ~f:(fun acc { f = T v } ->
            Map.set
              acc
              ~key:(M.Typed_variant.name v)
              ~data:(of_parser (M.parser_for_variant v)))
        in
        let identifiers =
          List.fold
            M.Typed_variant.Packed.all
            ~init:String.Map.empty
            ~f:(fun acc { f = T v } ->
            Map.set acc ~key:(M.Typed_variant.name v) ~data:(M.identifier_for_variant v))
        in
        Query_based_variant
          { override_namespace; constructor_declarations; identifiers; key }
      | Optional_query_fields { t } -> Optional_query_fields { t = of_parser t }
    ;;

    let rec is_multiple = function
      | From_query_many _ | From_remaining_path _ -> true
      | Unit
      | From_query_required _
      | From_query_optional _
      | From_query_optional_with_default _
      | From_path _
      | Record _
      | Variant _
      | Query_based_variant _ -> false
      | Project t
      | With_prefix { t; _ }
      | With_remaining_path { t; _ }
      | Optional_query_fields { t; _ } -> is_multiple t
    ;;

    let rec has_enough_information_on_its_own = function
      | Project t | Optional_query_fields { t; _ } -> has_enough_information_on_its_own t
      | From_query_required { override_key; _ }
      | From_query_optional { override_key; _ }
      | From_query_optional_with_default { override_key; _ }
      | From_query_many { override_key; _ } -> Option.is_some override_key
      | Unit
      | From_path _
      | From_remaining_path _
      | With_prefix _
      | With_remaining_path _
      | Record _
      | Variant _
      | Query_based_variant _ -> true
    ;;
  end

  let eval t =
    Eval.eval
      ~more_path_parsing_allowed:true
      ~current_namespace:[]
      ~inferred_name_from_parent:None
      ~parent_namespace:[]
      t
  ;;

  let slash_regexp = Re.Str.regexp "/"
  let unicode_slash_regexp = Re.Str.regexp "%2F"

  let sanitize_slashes s =
    let url_unicode_slash = "%2F" in
    Re.Str.global_replace slash_regexp url_unicode_slash s
  ;;

  let parse_unicode_slashes s = Re.Str.global_replace unicode_slash_regexp "/" s

  let eval ?(encoding_behavior : Percent_encoding_behavior.t = Correct) (t : 'a t)
    : (Components.t, 'a Parse_result.t) Projection.t
    =
    let projection : (Components.t, 'a Parse_result.t) Projection.t = eval t in
    let parse_exn (components : Components.t) =
      projection.parse_exn
        (match encoding_behavior with
         | Legacy_incorrect ->
           { components with path = List.map ~f:parse_unicode_slashes components.path }
         | Correct -> components)
    in
    let unparse (result : 'a Parse_result.t) =
      match encoding_behavior with
      | Legacy_incorrect ->
        let components = projection.unparse result in
        { components with path = List.map ~f:sanitize_slashes components.path }
      | Correct -> projection.unparse result
    in
    { Projection.parse_exn; unparse }
  ;;

  let eval_for_uri ?encoding_behavior (t : 'a t) : (Uri.t, 'a Parse_result.t) Projection.t
    =
    let projection = eval ?encoding_behavior t in
    let parse_exn (uri : Uri.t) =
      projection.parse_exn (Components.of_uri ?encoding_behavior uri)
    in
    let unparse (result : 'a Parse_result.t) =
      Components.to_uri ?encoding_behavior (projection.unparse result)
    in
    { Projection.parse_exn; unparse }
  ;;

  module Query_tag = struct
    type t =
      { key : string
      ; value : string
      }
    [@@deriving compare, sexp]

    let to_string t = t.key ^ "=" ^ t.value
  end

  module Url_shape = struct
    module T = struct
      type t =
        { path : [ `Pattern of string | `Value of (string[@compare.ignore]) ] list
        ; query : Query_tag.t list
        }
      [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)

    let to_string t =
      let path =
        String.concat
          ~sep:"/"
          (List.map t.path ~f:(function `Pattern x | `Value x -> x))
      in
      let query =
        if List.is_empty t.query
        then ""
        else
          "?"
          ^ String.concat
              ~sep:"&"
              (List.sort
                 (List.map t.query ~f:Query_tag.to_string)
                 ~compare:String.compare)
      in
      "/" ^ path ^ query
    ;;
  end

  let all_url_shapes (type a) (t : a T.t) : Url_shape.t list =
    let skeleton = Skeleton.of_parser t in
    let rec all_url_shapes
      (t : Skeleton.t)
      ~(current_shape : Url_shape.t)
      ~current_namespace
      ~parent_namespace
      ~inferred_name_from_parent
      =
      let wrap_if_multiple s =
        if Skeleton.is_multiple t then [%string "<multiple%{s}>"] else s
      in
      match t with
      | Unit -> [ current_shape ]
      | From_query_required { override_key; value_parser }
      | From_query_many { override_key; value_parser } ->
        let key =
          Eval.read_query_key
            ~override_key
            ~inferred_name_from_parent
            ~current_namespace
            ~f:Fn.id
        in
        [ { Url_shape.path = current_shape.path
          ; query =
              { key
              ; value = Value_parser.Skeleton.to_summary value_parser |> wrap_if_multiple
              }
              :: current_shape.query
          }
        ]
      | From_query_optional { override_key; value_parser }
      | From_query_optional_with_default { override_key; value_parser } ->
        let key =
          Eval.read_query_key
            ~override_key
            ~inferred_name_from_parent
            ~current_namespace
            ~f:Fn.id
        in
        [ { Url_shape.path = current_shape.path
          ; query =
              { key
              ; value =
                  [%string "<optional%{Value_parser.Skeleton.to_summary value_parser}>"]
              }
              :: current_shape.query
          }
        ]
      | Project t ->
        all_url_shapes
          ~current_shape
          ~current_namespace
          ~parent_namespace
          ~inferred_name_from_parent
          t
      | From_path value_parser | Skeleton.From_remaining_path value_parser ->
        let value = wrap_if_multiple (Value_parser.Skeleton.to_summary value_parser) in
        [ { Url_shape.path = current_shape.path @ [ `Value value ]
          ; query = current_shape.query
          }
        ]
      | With_prefix { prefix; t } | With_remaining_path { needed_path = prefix; t } ->
        let url_shape_with_pattern =
          let path = current_shape.path @ List.map prefix ~f:(fun x -> `Pattern x) in
          { current_shape with path }
        in
        all_url_shapes
          ~current_shape:url_shape_with_pattern
          ~current_namespace
          ~parent_namespace
          ~inferred_name_from_parent
          t
      | Record { override_namespace; label_declarations; path_order } ->
        let parse_order =
          let path_order_set = String.Set.of_list path_order in
          let fields_that_don't_appear_in_path_order =
            List.filter (Map.keys label_declarations) ~f:(fun field ->
              not (Set.mem path_order_set field))
          in
          path_order @ fields_that_don't_appear_in_path_order
        in
        List.fold parse_order ~init:[ current_shape ] ~f:(fun acc field_name ->
          List.concat_map acc ~f:(fun shape ->
            all_url_shapes
              (Map.find_exn label_declarations field_name)
              ~current_shape:shape
              ~current_namespace:
                (Eval.namespace_for_record_field
                   ~current_namespace
                   ~override_namespace
                   ~parent_namespace)
              ~parent_namespace:[ field_name ]
              ~inferred_name_from_parent:(Some field_name)))
      | Variant { constructor_declarations; override_namespace; patterns = _ } ->
        let namespace =
          Eval.namespace_for_record_field
            ~current_namespace
            ~override_namespace
            ~parent_namespace
        in
        List.concat_map
          (Map.to_alist constructor_declarations)
          ~f:(fun (constructor_name, declaration) ->
          all_url_shapes
            declaration
            ~current_shape
            ~current_namespace:namespace
            ~parent_namespace:[ constructor_name ]
            ~inferred_name_from_parent:(Some constructor_name))
      | Query_based_variant
          { constructor_declarations; override_namespace; identifiers; key } ->
        let namespace =
          Eval.namespace_for_record_field
            ~current_namespace
            ~override_namespace
            ~parent_namespace
        in
        List.concat_map
          (Map.to_alist constructor_declarations)
          ~f:(fun (constructor_name, declaration) ->
          let current_shape =
            { current_shape with
              query =
                { Query_tag.key
                ; value =
                    Map.find identifiers constructor_name
                    |> Option.value ~default:"<string>"
                }
                :: current_shape.query
            }
          in
          all_url_shapes
            declaration
            ~current_shape
            ~current_namespace:namespace
            ~parent_namespace:[ constructor_name ]
            ~inferred_name_from_parent:(Some constructor_name))
      | Optional_query_fields { t; _ } ->
        current_shape
        :: all_url_shapes
             t
             ~current_shape
             ~current_namespace
             ~parent_namespace
             ~inferred_name_from_parent
    in
    all_url_shapes
      skeleton
      ~current_shape:{ Url_shape.path = []; query = [] }
      ~current_namespace:[]
      ~parent_namespace:[]
      ~inferred_name_from_parent:None
  ;;

  let check_no_duplicate_keys (all_url_shapes : Url_shape.t list) =
    let duplicate_query_keys_by_url =
      List.filter_map all_url_shapes ~f:(fun url_shape ->
        match
          List.find_all_dups
            ~compare:String.compare
            (List.map url_shape.query ~f:(fun query -> query.key))
        with
        | [] -> None
        | dups -> Some (Url_shape.to_string url_shape, dups))
    in
    match duplicate_query_keys_by_url with
    | [] -> ()
    | _ ->
      raise_s
        [%message
          "Duplicate query keys found! This probably occurred due at least one [~key] \
           being renamed to the same string in a [from_query_*] parser. Another \
           possibility is that a renamed key conflicted with an inferred parser."
            (duplicate_query_keys_by_url : (string * string list) list)]
  ;;

  let check_no_duplicate_urls (all_url_shapes : Url_shape.t list) =
    let normalized_url_shapes =
      List.map all_url_shapes ~f:(fun shape ->
        let query = List.sort ~compare:Query_tag.compare shape.query in
        { shape with query })
    in
    match List.find_all_dups normalized_url_shapes ~compare:Url_shape.compare with
    | [] -> ()
    | duplicate_urls ->
      let duplicate_urls = List.map duplicate_urls ~f:Url_shape.to_string in
      raise_s
        [%message
          "Ambiguous, duplicate urls expressed in parser! This was probably caused due \
           to conflicting renames with [with_prefix] or [with_remaining_path]."
            (duplicate_urls : string list)]
  ;;

  let check_has_enough_information_on_its_own (t : 'a T.t) =
    match Skeleton.has_enough_information_on_its_own (Skeleton.of_parser t) with
    | true -> ()
    | false -> Eval.raise_can't_infer_key_error ()
  ;;

  let rec check_that_path_orders_are_ok : type a. a T.t -> unit = function
    | Unit -> ()
    | Project { input; _ } -> check_that_path_orders_are_ok input
    | Optional_query_fields { t; _ } -> check_that_path_orders_are_ok t
    | From_query_required _ -> ()
    | From_query_optional _ -> ()
    | From_query_optional_with_default _ -> ()
    | From_query_many _ -> ()
    | From_path _ -> ()
    | From_remaining_path _ -> ()
    | With_prefix { t; _ } -> check_that_path_orders_are_ok t
    | With_remaining_path { t; _ } -> check_that_path_orders_are_ok t
    | Record { record_module; _ } ->
      let module M =
        (val record_module : Record.Cached_s with type Typed_field.derived_on = a)
      in
      Eval.raise_if_path_order_has_duplicates (module M);
      Eval.raise_if_path_field_does_not_have_order (module M);
      List.iter M.Typed_field.Packed.all ~f:(fun { f = T f } ->
        check_that_path_orders_are_ok (M.parser_for_field f))
    | Variant { variant_module; _ } ->
      let module M =
        (val variant_module : Variant.Cached_s with type Typed_variant.derived_on = a)
      in
      List.iter M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
        check_that_path_orders_are_ok (M.parser_for_variant v))
    | Query_based_variant { variant_module; _ } ->
      let module M =
        (val variant_module
            : Query_based_variant.S with type Typed_variant.derived_on = a)
      in
      List.iter M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
        check_that_path_orders_are_ok (M.parser_for_variant v))
  ;;

  let check_more_path_parsing_allowed ~has_seen_end_of_path =
    if has_seen_end_of_path
    then
      raise_s
        [%message
          "Error! There cannot be path parsers inside of a [with_remaining_path] \
           combinator! The reason for this is that there won't be any values after that \
           parser!"]
  ;;

  let rec check_that_with_remaining_path_has_no_path_parsers_after_it
    : type a. has_seen_end_of_path:bool -> a T.t -> bool
    =
    let ( || ) a b =
      (* NOTE: This is silly, but avoids (||)'s short-circuiting. *)
      a || b
    in
    fun ~has_seen_end_of_path t ->
      match t with
      | Unit -> has_seen_end_of_path
      | Project { input; _ } ->
        check_that_with_remaining_path_has_no_path_parsers_after_it
          ~has_seen_end_of_path
          input
      | Optional_query_fields { t; _ } ->
        check_that_with_remaining_path_has_no_path_parsers_after_it
          ~has_seen_end_of_path
          t
      | From_query_required _ -> has_seen_end_of_path
      | From_query_optional _ -> has_seen_end_of_path
      | From_query_optional_with_default _ -> has_seen_end_of_path
      | From_query_many _ -> has_seen_end_of_path
      | From_path _ ->
        check_more_path_parsing_allowed ~has_seen_end_of_path;
        has_seen_end_of_path
      | From_remaining_path _ ->
        check_more_path_parsing_allowed ~has_seen_end_of_path;
        true
      | With_prefix { t; prefix } ->
        if not (List.is_empty prefix)
        then check_more_path_parsing_allowed ~has_seen_end_of_path;
        check_that_with_remaining_path_has_no_path_parsers_after_it
          ~has_seen_end_of_path
          t
      | With_remaining_path { t; needed_path } ->
        if not (List.is_empty needed_path)
        then check_more_path_parsing_allowed ~has_seen_end_of_path;
        check_that_with_remaining_path_has_no_path_parsers_after_it
          ~has_seen_end_of_path:true
          t
      | Record { record_module; _ } ->
        let module M =
          (val record_module : Record.Cached_s with type Typed_field.derived_on = a)
        in
        List.fold
          ~init:has_seen_end_of_path
          M.path_order
          (* NOTE: This check assumes that the [check_path_orders_are_sane] check has run
             beforehand. It is safe to not traverse parsers that do not have any paths
             beforehand. *)
          ~f:(fun has_seen_end_of_path { f = T f } ->
          has_seen_end_of_path
          || check_that_with_remaining_path_has_no_path_parsers_after_it
               ~has_seen_end_of_path
               (M.parser_for_field f))
      | Variant { variant_module; _ } ->
        let module M =
          (val variant_module : Variant.Cached_s with type Typed_variant.derived_on = a)
        in
        List.fold
          ~init:has_seen_end_of_path
          M.Typed_variant.Packed.all
          ~f:(fun out { f = T v } ->
          (* NOTE: the difference between variant's and records is subtle but important.
               records will have ALL of their parser's run, while variant's will only have
               one of their parsers run.
            *)
          out
          || check_that_with_remaining_path_has_no_path_parsers_after_it
               ~has_seen_end_of_path
               (M.parser_for_variant v))
      | Query_based_variant { variant_module; _ } ->
        let module M =
          (val variant_module
              : Query_based_variant.S with type Typed_variant.derived_on = a)
        in
        List.fold
          ~init:has_seen_end_of_path
          M.Typed_variant.Packed.all
          ~f:(fun out { f = T v } ->
          out
          || check_that_with_remaining_path_has_no_path_parsers_after_it
               ~has_seen_end_of_path
               (M.parser_for_variant v))
  ;;

  let rec check_that_there_are_no_ambiguous_parsers_at_any_level : type a. a T.t -> unit =
    fun t ->
    match t with
    | Unit
    | From_query_required _
    | From_query_optional _
    | From_query_optional_with_default _
    | From_query_many _
    | From_path _
    | From_remaining_path _
    | With_prefix _
    | With_remaining_path _
    | Record _ -> ()
    | Project { input; _ } -> check_that_there_are_no_ambiguous_parsers_at_any_level input
    | Optional_query_fields { t; _ } ->
      check_that_there_are_no_ambiguous_parsers_at_any_level t
    | Variant { variant_module; _ } ->
      let module M =
        (val variant_module : Variant.Cached_s with type Typed_variant.derived_on = a)
      in
      let all_patterns =
        List.map M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
          M.pattern_for_variant v)
      in
      let dup_patterns = List.find_all_dups ~compare:Path_pattern.compare all_patterns in
      (match dup_patterns with
       | [] -> ()
       | duplicate_patterns ->
         raise_s
           [%message
             "Duplicate patterns found!" (duplicate_patterns : Path_pattern.t list)])
    | Query_based_variant { variant_module; _ } ->
      let module M =
        (val variant_module
            : Query_based_variant.S with type Typed_variant.derived_on = a)
      in
      let all_identifiers =
        List.map M.Typed_variant.Packed.all ~f:(fun { f = T v } ->
          M.identifier_for_variant v)
      in
      let duplicate_identifiers =
        List.find_all_dups ~compare:[%compare: string] all_identifiers
      in
      (match duplicate_identifiers with
       | [] -> ()
       | duplicate_identifiers ->
         raise_s
           [%message
             "Duplicate identifiers to distinguish variants found!"
               (duplicate_identifiers : string list)])
  ;;

  let run_check ~name ~f = name, Or_error.try_with f

  let check_ok t =
    let checks_that_don't_need_all_information_to_suceed =
      [ run_check ~name:"Enough information needed to parse check" ~f:(fun () ->
          check_has_enough_information_on_its_own t)
      ; run_check ~name:"Sane path orders check" ~f:(fun () ->
          check_that_path_orders_are_ok t)
      ; run_check ~name:"Remaining path does not block other parsers check" ~f:(fun () ->
          (ignore : bool -> unit)
            (check_that_with_remaining_path_has_no_path_parsers_after_it
               ~has_seen_end_of_path:false
               t))
      ; run_check
          ~name:"Ambiguous choices for picking variant constructor check"
          ~f:(fun () -> check_that_there_are_no_ambiguous_parsers_at_any_level t)
      ]
    in
    let checks_that_need_all_information_to_succeed =
      try
        let all_url_shapes = all_url_shapes t in
        [ run_check ~name:"Duplicate keys check" ~f:(fun () ->
            check_no_duplicate_keys all_url_shapes)
        ; run_check ~name:"Duplicate urls check" ~f:(fun () ->
            check_no_duplicate_urls all_url_shapes)
        ]
      with
      | _ -> []
    in
    checks_that_don't_need_all_information_to_suceed
    @ checks_that_need_all_information_to_succeed
  ;;

  let sexp_of_t t = Skeleton.sexp_of_t (Skeleton.of_parser t)

  let all_urls t =
    List.map (all_url_shapes t) ~f:Url_shape.to_string
    |> List.sort ~compare:String.compare
  ;;

  let check_ok_and_print_urls_or_errors (t : 'a T.t) =
    let checks = check_ok t in
    let errors =
      List.filter_map checks ~f:(function
        | _, Ok () -> None
        | name, Error e -> Some (name, e))
    in
    match errors with
    | [] ->
      print_endline "URL parser looks good!";
      let all_url_shapes = all_urls t in
      let columns = [ Ascii_table_kernel.Column.create "All urls" Fn.id ] in
      print_endline
        (Ascii_table_kernel.to_string_noattr
           ~limit_width_to:Int.max_value
           columns
           all_url_shapes
           ~bars:`Unicode)
    | errors ->
      print_endline "Error with parser.";
      let columns =
        [ Ascii_table_kernel.Column.create "Check name" Tuple2.get1
        ; Ascii_table_kernel.Column.create
            "Error message"
            (Fn.compose Error.to_string_hum Tuple2.get2)
        ]
      in
      print_endline
        (Ascii_table_kernel.to_string_noattr
           ~limit_width_to:Int.max_value
           columns
           errors
           ~bars:`Unicode)
  ;;

  let to_string (t : 'a t) : ('a -> string) Staged.t =
    let projection = eval_for_uri ~encoding_behavior:Correct t in
    let to_string a =
      let parsed = projection.unparse (Parse_result.create a) in
      "/" ^ Uri.to_string parsed
    in
    Staged.stage to_string
  ;;
end

module Versioned_parser = struct
  type 'curr_version t =
    | Non_typed_parser of (Components.t, 'curr_version) Projection.t
    | First_typed_parser of 'curr_version Parser.t
    | New_parser :
        { current_parser : 'curr_version Parser.t
        ; map : 'prev_version -> 'curr_version
        ; previous_parser : 'prev_version t
        }
        -> 'curr_version t

  let of_non_typed_parser projection = Non_typed_parser projection
  let first_parser parser = First_typed_parser parser

  let new_parser new_parser ~previous ~f =
    New_parser { current_parser = new_parser; map = f; previous_parser = previous }
  ;;

  let eval_non_typed_parser (projection : (Components.t, 'a) Projection.t) =
    let parse_exn (components : Components.t) =
      let result = projection.parse_exn components in
      { Parse_result.result; remaining = Components.empty }
    in
    let unparse { Parse_result.result; _ } = projection.unparse result in
    { Projection.parse_exn; unparse }
  ;;

  let rec eval
    : type a.
      ?encoding_behavior:Percent_encoding_behavior.t
      -> a t
      -> (Components.t, a Parse_result.t) Projection.t
    =
    fun ?encoding_behavior -> function
    | Non_typed_parser projection -> eval_non_typed_parser projection
    | First_typed_parser parser -> Parser.eval ?encoding_behavior parser
    | New_parser { current_parser; map; previous_parser } ->
      let current_projection = Parser.eval ?encoding_behavior current_parser in
      let previous_projection = eval ?encoding_behavior previous_parser in
      let parse_exn (components : Components.t) =
        try current_projection.parse_exn components with
        | error ->
          print_s
            [%message
              "URL unrecognized, maybe this is an old URL? Attempting to parse with a \
               previous URL parser. Here's the error of the current parser:"
                (error : Exn.t)];
          let result = previous_projection.parse_exn components in
          { Parse_result.result = map result.result; remaining = result.remaining }
      in
      let unparse result = current_projection.unparse result in
      { Projection.parse_exn; unparse }
  ;;

  let eval_for_uri ?encoding_behavior (t : 'a t) : (Uri.t, 'a Parse_result.t) Projection.t
    =
    let projection = eval ?encoding_behavior t in
    let parse_exn (uri : Uri.t) =
      projection.parse_exn (Components.of_uri ?encoding_behavior uri)
    in
    let unparse (result : 'a Parse_result.t) =
      Components.to_uri ?encoding_behavior (projection.unparse result)
    in
    { Projection.parse_exn; unparse }
  ;;

  let to_string (t : 'a t) : ('a -> string) Staged.t =
    let projection = eval_for_uri ~encoding_behavior:Correct t in
    let to_string a =
      let parsed = projection.unparse (Parse_result.create a) in
      "/" ^ Uri.to_string parsed
    in
    Staged.stage to_string
  ;;

  type packed_parser = T : 'a Parser.t -> packed_parser

  let rec to_parser_list : type a. a t -> [ `Typed of packed_parser | `Non_typed ] list
    = function
    | Non_typed_parser _ -> [ `Non_typed ]
    | First_typed_parser parser -> [ `Typed (T parser) ]
    | New_parser { current_parser; map = _; previous_parser } ->
      `Typed (T current_parser) :: to_parser_list previous_parser
  ;;

  let check_ok_and_print_urls_or_errors (t : 'a t) =
    let parsers = to_parser_list t in
    let rec loop = function
      | [] -> ()
      | `Typed (T top) :: next :: tl ->
        Parser.check_ok_and_print_urls_or_errors top;
        print_endline "       |";
        print_endline "falls back to";
        print_endline "       |";
        print_endline "       v";
        print_endline "";
        loop (next :: tl)
      | `Typed (T top) :: tl ->
        Parser.check_ok_and_print_urls_or_errors top;
        loop tl
      | `Non_typed :: _ ->
        print_endline "Cannot perform static checks on non-typed-fields parser"
    in
    loop parsers
  ;;

  let all_urls t =
    let rec helper : type a. a t -> string list = function
      | Non_typed_parser _ -> []
      | First_typed_parser parser -> Parser.all_urls parser
      | New_parser { current_parser; previous_parser; map = _ } ->
        Parser.all_urls current_parser @ helper previous_parser
    in
    List.dedup_and_sort ~compare:String.compare (helper t)
  ;;
end
