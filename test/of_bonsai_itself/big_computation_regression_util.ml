open! Core
open Bonsai_test

module For_proc = struct
  open Bonsai.Let_syntax

  let basic : height:int -> width:int -> String.Set.t Bonsai.Computation.t =
    fun ~height ~width ->
    Fn.apply_n_times
      ~n:height
      (fun rem ->
        let%sub _state, _set_state =
          Fn.apply_n_times
            ~n:(width - 1)
            (fun ret ->
              let%sub _state, _set_state = Bonsai.state () in
              ret)
            (let%sub state, set_state = Bonsai.state () in
             let%arr state = state
             and set_state = set_state in
             state, set_state)
        in
        let%sub path_id = Bonsai.path_id in
        let%sub rem = rem in
        let%arr rem = rem
        and path_id = path_id in
        Set.add rem path_id)
      (let%sub path_id = Bonsai.path_id in
       let%arr path_id = path_id in
       String.Set.singleton path_id)
  ;;

  let with_assoc
    :  height:int -> width:int -> num_assocs:int
    -> String.Set.t String.Map.t Bonsai.Computation.t
    =
    fun ~height ~width ~num_assocs ->
    let%sub paths = basic ~height ~width in
    let%sub paths_first_two =
      let%arr paths = paths in
      Set.to_list paths |> fun x -> List.take x num_assocs |> Set.of_list (module String)
    in
    Bonsai.assoc_set (module String) paths_first_two ~f:(fun _ -> basic ~height ~width)
  ;;

  let with_switch : height:int -> width:int -> int Bonsai.Computation.t =
    fun ~height ~width ->
    let%sub paths = basic ~height ~width in
    let%sub paths_list =
      let%arr paths = paths in
      Set.to_list paths
    in
    match%sub paths_list with
    | [] -> Bonsai.const 0
    | [ x ] ->
      let%arr x = x in
      String.length x
    | _ ->
      let%sub paths = basic ~height ~width in
      let%arr paths = paths in
      Set.length paths
  ;;
end

module For_cont = struct
  module Bonsai = Bonsai.Cont
  open Bonsai.Let_syntax

  let basic : height:int -> width:int -> Bonsai.graph -> String.Set.t Bonsai.t =
    fun ~height ~width ->
    Fn.apply_n_times
      ~n:height
      (fun rem graph ->
        let _state, _set_state =
          Fn.apply_n_times
            ~n:(width - 1)
            (fun ret ->
              let _state, _set_state = Bonsai.state () graph in
              ret)
            (let state, set_state = Bonsai.state () graph in
             state, set_state)
        in
        let path_id = Bonsai.path_id graph in
        let rem = rem graph in
        let%arr rem = rem
        and path_id = path_id in
        Set.add rem path_id)
      (fun graph ->
        let path_id = Bonsai.path_id graph in
        let%arr path_id = path_id in
        String.Set.singleton path_id)
  ;;

  let with_assoc
    :  height:int -> width:int -> num_assocs:int -> Bonsai.graph
    -> String.Set.t String.Map.t Bonsai.t
    =
    fun ~height ~width ~num_assocs graph ->
    let paths = basic ~height ~width graph in
    let paths_first_two =
      let%arr paths = paths in
      Set.to_list paths |> fun x -> List.take x num_assocs |> Set.of_list (module String)
    in
    Bonsai.assoc_set
      (module String)
      paths_first_two
      ~f:(fun _ -> basic ~height ~width)
      graph
  ;;

  let with_switch : height:int -> width:int -> Bonsai.graph -> int Bonsai.t =
    fun ~height ~width graph ->
    let paths = basic ~height ~width graph in
    let paths_list =
      let%arr paths = paths in
      Set.to_list paths
    in
    match%sub paths_list with
    | [] -> return 0
    | [ x ] ->
      let%arr x = x in
      String.length x
    | _ ->
      let%arr paths = basic ~height ~width graph in
      Set.length paths
  ;;
end

let result_spec ~only_print_lengths
  : (module Result_spec.S with type t = String.Set.t and type incoming = Nothing.t)
  =
  let module Result_spec = struct
    type t = String.Set.t
    type incoming = Nothing.t

    let view s =
      match only_print_lengths with
      | false -> Sexp.to_string_hum [%sexp (s : String.Set.t)]
      | true ->
        Sexp.to_string_hum [%sexp (Set.to_list s |> List.map ~f:String.length : int list)]
    ;;

    let incoming _ (incoming : incoming) =
      match incoming with
      | _ -> .
    ;;
  end
  in
  (module Result_spec)
;;

let lengths_result_spec = result_spec ~only_print_lengths:true
let values_result_spec = result_spec ~only_print_lengths:false
