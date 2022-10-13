open! Core
open Bonsai_quickcheck_internal
module Buf = Indentation_buffer

let default_buf_size = 1024
let data_to_string = real_data_to_string

let prep buffer =
  Buf.newline buffer;
  Buf.indent buffer
;;

let leave buffer =
  Buf.newline buffer;
  Buf.dedent buffer;
  Buf.string buffer ") "
;;

let rec write_comparator : type a cmp. buffer:Buf.t -> (a, cmp) Witness.t -> unit =
  fun ~buffer witness ->
  match witness with
  | Unit -> Buf.string buffer "(module Unit) "
  | Int -> Buf.string buffer "(module Int) "
  | Either (first_witness, second_witness) ->
    Buf.string buffer "(";
    prep buffer;
    Buf.string buffer "let module First = ";
    write_comparator ~buffer first_witness;
    Buf.newline buffer;
    Buf.string buffer "let module Second = ";
    write_comparator ~buffer second_witness;
    Buf.newline buffer;
    Buf.string buffer "(module struct";
    prep buffer;
    Buf.string buffer "type t = (First.t, Second.t) Either.t";
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string
      buffer
      {|type comparator_witness = (First.comparator_witness, Second.comparator_witness) Either.comparator_witness|};
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string
      buffer
      {|let comparator : (t, comparator_witness) Comparator.t = Either.comparator First.comparator Second.comparator|};
    Buf.newline buffer;
    Buf.dedent buffer;
    Buf.string buffer " end)";
    leave buffer
  | Tuple (first_witness, second_witness) ->
    Buf.string buffer "(";
    prep buffer;
    Buf.string buffer "let module First = ";
    write_comparator ~buffer first_witness;
    Buf.newline buffer;
    Buf.string buffer "let module Second = ";
    write_comparator ~buffer second_witness;
    Buf.newline buffer;
    Buf.string buffer "(module struct";
    prep buffer;
    Buf.string buffer "type t = (First.t, Second.t) Tuple2.t";
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string
      buffer
      {|type comparator_witness = (First.comparator_witness, Second.comparator_witness) Tuple2.comparator_witness|};
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string
      buffer
      {|let comparator : (t, comparator_witness) Comparator.t = Tuple2.comparator First.comparator Second.comparator|};
    Buf.newline buffer;
    Buf.dedent buffer;
    Buf.string buffer " end)";
    leave buffer
  | Map (_, _) -> assert false
  | Effect_func _ -> assert false
;;

let rec write_model : type a cmp. buffer:Buf.t -> (a, cmp) Witness.t -> unit =
  fun ~buffer witness ->
  match witness with
  | Unit -> Buf.string buffer "(module Unit) "
  | Int -> Buf.string buffer "(module Int) "
  | Either (first_witness, second_witness) ->
    Buf.string buffer "(";
    prep buffer;
    Buf.string buffer "let module First = ";
    write_model ~buffer first_witness;
    Buf.newline buffer;
    Buf.string buffer "let module Second = ";
    write_model ~buffer second_witness;
    Buf.newline buffer;
    Buf.string buffer "(module struct";
    prep buffer;
    Buf.string buffer "type t = (First.t, Second.t) Either.t [@@deriving sexp]";
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string buffer "let equal = Either.equal First.equal Second.equal";
    Buf.newline buffer;
    Buf.dedent buffer;
    Buf.string buffer " end)";
    leave buffer
  | Tuple (first_witness, second_witness) ->
    Buf.string buffer "(";
    prep buffer;
    Buf.string buffer "let module First = ";
    write_model ~buffer first_witness;
    Buf.newline buffer;
    Buf.string buffer "let module Second = ";
    write_model ~buffer second_witness;
    Buf.newline buffer;
    Buf.string buffer "(module struct";
    prep buffer;
    Buf.string buffer "type t = (First.t, Second.t) Tuple2.t [@@deriving sexp]";
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string buffer "let equal = Tuple2.equal First.equal Second.equal";
    Buf.newline buffer;
    Buf.dedent buffer;
    Buf.string buffer " end)";
    leave buffer
  | Map (key_witness, value_witness) ->
    Buf.string buffer "(";
    prep buffer;
    Buf.string buffer "let module K = ";
    write_comparator ~buffer key_witness;
    Buf.newline buffer;
    Buf.string buffer "let module V = ";
    write_model ~buffer value_witness;
    Buf.newline buffer;
    Buf.string buffer "(module struct";
    prep buffer;
    Buf.string buffer "type t = (K.t, V.t, K.comparator_witness) Map.t [@@deriving sexp]";
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string buffer "let equal = Map.equal V.equal";
    Buf.newline buffer;
    Buf.dedent buffer;
    Buf.string buffer " end)";
    leave buffer
  | Effect_func inner_witness ->
    Buf.string buffer "(";
    prep buffer;
    Buf.string buffer "let module M = ";
    write_model ~buffer inner_witness;
    Buf.newline buffer;
    Buf.string buffer "(module struct";
    prep buffer;
    Buf.string buffer "type t = M.t -> unit Bonsai.Effect.t [@@deriving sexp]";
    Buf.newline buffer;
    Buf.newline buffer;
    Buf.string buffer "let equal _ _ = true";
    Buf.newline buffer;
    Buf.dedent buffer;
    Buf.string buffer " end)";
    leave buffer
;;

let rec write_data : type a cmp. buffer:Buf.t -> (a, cmp) Witness.t -> a -> unit =
  fun ~buffer witness data ->
  match witness with
  | Unit | Int -> Buf.string buffer (data_to_string witness data)
  | Either (first_witness, second_witness) ->
    Buf.string buffer "(Either.";
    let () =
      match data with
      | First first ->
        Buf.string buffer "First ";
        write_data ~buffer first_witness first
      | Second second ->
        Buf.string buffer "Second ";
        write_data ~buffer second_witness second
    in
    Buf.string buffer ")"
  | Tuple (first_witness, second_witness) ->
    let first, second = data in
    Buf.string buffer "(";
    write_data ~buffer first_witness first;
    Buf.string buffer ", ";
    write_data ~buffer second_witness second;
    Buf.string buffer ")"
  | Map (key_witness, data_witness) ->
    Buf.string buffer "Map.of_alist_exn ";
    write_comparator ~buffer key_witness;
    Buf.string buffer "[";
    let alist = Map.to_alist data in
    List.iter alist ~f:(fun entry ->
      write_data ~buffer (Tuple (key_witness, data_witness)) entry;
      Buf.string buffer "; ");
    Buf.string buffer "]"
  | Effect_func _ -> Buf.string buffer "fun _ -> Bonsai.Effect.Ignore"
;;

let rec write_function
  : type input output cmp.
    buffer:Buf.t -> (input, output) Function.t -> (output, cmp) Witness.t -> unit
  =
  fun ~buffer f witness ->
  match f with
  | Identity -> Buf.string buffer "fun x -> x"
  | Const output ->
    Buf.string buffer "fun _ -> ";
    write_data ~buffer witness output
  | Add_const add -> Buf.string buffer [%string "fun x -> x + %{add#Int}"]
  | Snd -> Buf.string buffer "fun (_, x) -> x"
  | Map_tuple (func1, func2) ->
    (match witness with
     | Tuple (first_witness, second_witness) ->
       Buf.string buffer "fun (x, y) -> (";
       write_function ~buffer func1 first_witness;
       Buf.string buffer ") x, (";
       write_function ~buffer func2 second_witness;
       Buf.string buffer ") y"
     | _ -> assert false)
  | Make_either which ->
    (match which with
     | `First -> Buf.string buffer "fun (x, _) -> First x"
     | `Second -> Buf.string buffer "fun (_, x) -> Second x")
;;

let rec write_value : type a cmp. buffer:Buf.t -> a Value.t -> (a, cmp) Witness.t -> unit =
  fun ~buffer value witness ->
  match value with
  | Return data ->
    Buf.string buffer "Value.return (";
    prep buffer;
    write_data ~buffer witness data;
    leave buffer
  | Map (inner, inner_witness, f) ->
    Buf.string buffer [%string "Value.map ("];
    prep buffer;
    write_value ~buffer inner inner_witness;
    leave buffer;
    Buf.string buffer "~f:(";
    write_function ~buffer f witness;
    Buf.string buffer ")"
  | Real_value value ->
    let { Bonsai.Private.Value.id; _ } = Bonsai.Private.reveal_value value in
    let id_string =
      Type_equal.Id.uid id |> Type_equal.Id.Uid.sexp_of_t |> Sexp.to_string_hum
    in
    Buf.string buffer [%string "x%{id_string}"]
  | Var data ->
    Buf.string buffer "Value.return (";
    prep buffer;
    Buf.string buffer "Bonsai.Var.value (";
    prep buffer;
    write_data ~buffer witness data;
    leave buffer;
    leave buffer
  | Both { first; first_witness; second; second_witness } ->
    Buf.string buffer "Value.both ";
    prep buffer;
    Buf.string buffer "(";
    write_value ~buffer first first_witness;
    Buf.string buffer ")";
    Buf.newline buffer;
    Buf.string buffer "(";
    write_value ~buffer second second_witness;
    Buf.string buffer ")";
    Buf.newline buffer;
    Buf.dedent buffer
;;

let real_value_to_variable_name real_value =
  let { Bonsai.Private.Value.id; _ } = Bonsai.Private.reveal_value real_value in
  let id_string =
    Type_equal.Id.uid id |> Type_equal.Id.Uid.sexp_of_t |> Sexp.to_string_hum
  in
  [%string "x%{id_string}"]
;;

let rec write_computation
  : type a cmp. buffer:Buf.t -> a Computation.t -> (a, cmp) Witness.t -> unit
  =
  fun ~buffer computation witness ->
  let open Bonsai.Let_syntax in
  match computation with
  | Return value ->
    Buf.string buffer "return (";
    prep buffer;
    write_value ~buffer value witness;
    leave buffer
  | Subst (inner, inner_witness, f) ->
    let real_value = ref None in
    let (_ : unit Bonsai.Computation.t) =
      let%sub x = to_real_computation inner in
      real_value := Some x;
      Bonsai.const ()
    in
    let real_value = Option.value_exn !real_value in
    let id_string = real_value_to_variable_name real_value in
    Buf.string buffer [%string "let%sub %{id_string} = ("];
    prep buffer;
    write_computation ~buffer inner inner_witness;
    leave buffer;
    Buf.string buffer "in";
    Buf.newline buffer;
    let fake_computation = f (of_real_value real_value) in
    write_computation ~buffer fake_computation witness
  | Subst2 { tuple_computation; first_witness; second_witness; f } ->
    let real_first, real_second = ref None, ref None in
    let (_ : _ Bonsai.Computation.t) =
      let%sub x, y = to_real_computation tuple_computation in
      real_first := Some x;
      real_second := Some y;
      Bonsai.const ()
    in
    let real_first = Option.value_exn !real_first in
    let real_second = Option.value_exn !real_second in
    let fake_comp = f (of_real_value real_first) (of_real_value real_second) in
    let id_string_first = real_value_to_variable_name real_first in
    let id_string_second = real_value_to_variable_name real_second in
    Buf.string buffer [%string "let%sub %{id_string_first}, %{id_string_second} = ("];
    prep buffer;
    let tuple_witness = Witness.Tuple (first_witness, second_witness) in
    write_computation ~buffer tuple_computation tuple_witness;
    leave buffer;
    Buf.string buffer "in";
    Buf.newline buffer;
    write_computation ~buffer fake_comp witness
  | Switch { either_value; first_witness; second_witness; f_first; f_second } ->
    let real_either_value = to_real_value either_value in
    let either_witness = Witness.Either (first_witness, second_witness) in
    Buf.string buffer "match%sub (";
    write_value ~buffer either_value either_witness;
    Buf.string buffer ") with";
    Buf.newline buffer;
    let first_real_value = ref None in
    let second_real_value = ref None in
    let (_ : unit Bonsai.Computation.t) =
      match%sub real_either_value with
      | First first ->
        first_real_value := Some first;
        Bonsai.const ()
      | Second second ->
        second_real_value := Some second;
        Bonsai.const ()
    in
    let first_real_value = Option.value_exn !first_real_value in
    let second_real_value = Option.value_exn !second_real_value in
    let first_comp = f_first (of_real_value first_real_value) in
    let id_string = real_value_to_variable_name first_real_value in
    Buf.string buffer [%string "| First %{id_string} -> ("];
    prep buffer;
    write_computation ~buffer first_comp witness;
    leave buffer;
    Buf.newline buffer;
    let second_comp = f_second (of_real_value second_real_value) in
    let id_string = real_value_to_variable_name second_real_value in
    Buf.string buffer [%string "| Second %{id_string} -> ("];
    prep buffer;
    write_computation ~buffer second_comp witness;
    leave buffer
  | Assoc { map_value; key_witness; value_witness; f; result_witness } ->
    Buf.string buffer "Bonsai.assoc ";
    prep buffer;
    write_comparator ~buffer key_witness;
    let map_witness = Witness.Map (key_witness, value_witness) in
    Buf.newline buffer;
    Buf.string buffer "(";
    prep buffer;
    write_value ~buffer map_value map_witness;
    leave buffer;
    let map = to_real_value map_value in
    let module M = (val make_comparator_and_model key_witness) in
    let module K = struct
      include M

      let sexp_of_t = M.comparator.sexp_of_t
      let t_of_sexp _ = assert false
    end
    in
    let key_real_value = ref None in
    let data_real_value = ref None in
    let (_ : (_, unit, _) Map.t Bonsai.Computation.t) =
      Bonsai.assoc
        (module K)
        map
        ~f:(fun key data ->
          key_real_value := Some key;
          data_real_value := Some data;
          Bonsai.const ())
    in
    let key_real_value = Option.value_exn !key_real_value in
    let data_real_value = Option.value_exn !data_real_value in
    let key_id_string = real_value_to_variable_name key_real_value in
    let data_id_string = real_value_to_variable_name data_real_value in
    Buf.string buffer [%string "~f:(fun %{key_id_string} %{data_id_string} ->"];
    prep buffer;
    let fake_comp = f (of_real_value key_real_value) (of_real_value data_real_value) in
    write_computation ~buffer fake_comp result_witness;
    leave buffer;
    Buf.dedent buffer
  | State { default_model; default_witness } ->
    Buf.string buffer "Bonsai.state ";
    prep buffer;
    Buf.string buffer "~default_model:";
    write_data ~buffer default_witness default_model;
    Buf.newline buffer;
    write_model ~buffer default_witness;
    Buf.newline buffer;
    Buf.dedent buffer
;;


let packed_computation_to_ocaml_code ?(indent = 0) (packed : Computation.packed) =
  let (T { unpacked; witness }) = packed in
  let buffer = Buf.create default_buf_size in
  for _ = 0 to indent - 1 do
    Buf.indent buffer
  done;
  write_computation ~buffer unpacked witness;
  Buf.contents buffer
;;
