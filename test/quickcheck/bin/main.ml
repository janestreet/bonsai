open! Core
open Bonsai_quickcheck
module Buf = Indentation_buffer
module Q = Base_quickcheck

let random = Splittable_random.State.create (Random.State.make [| Random.bits () |])

let sexp_command =
  Command.basic
    ~summary:
      "Print the sexp of a randomly generated computation each time the user presses RET"
    (let%map_open.Command size = flag "size" (optional int) ~doc:"recursion depth" in
     fun () ->
       let size = Option.value size ~default:2 in
       while true do
         let (T { unpacked; _ }) =
           to_packed_real_computation
             (Q.Generator.generate
                Top_level_computation.quickcheck_generator
                ~size
                ~random)
         in
         unpacked
         |> Bonsai.Private.reveal_computation
         |> Bonsai.Private.Skeleton.Computation.of_computation
         |> Bonsai.Private.Skeleton.Computation.sanitize_for_testing
         |> Bonsai.Private.Skeleton.Computation.minimal_sexp_of_t
         |> print_s;
         (* Wait for the user to hit enter before continuing *)
         let (_ : string option) = In_channel.input_line In_channel.stdin in
         ()
       done)
;;

let code_command =
  Command.basic
    ~summary:
      "Print valid ocaml code for a randomly generated computation each time the user \
       presses RET"
    (let%map_open.Command size = flag "size" (optional int) ~doc:"recursion depth" in
     fun () ->
       let size = Option.value size ~default:2 in
       while true do
         let packed =
           Q.Generator.generate Top_level_computation.quickcheck_generator ~size ~random
         in
         let computation_string = packed_computation_to_ocaml_code packed in
         let buffer = Buf.create 1024 in
         Buf.string buffer computation_string;
         Buf.newline buffer;
         Buf.string buffer "-----------------------------------------------";
         print_endline (Buf.contents buffer);
         let (_ : string option) = In_channel.input_line In_channel.stdin in
         ()
       done)
;;

let hardcoded_command =
  Command.basic
    ~summary:"Print a hardcoded computation"
    (Command.Param.return (fun () ->
       (* Hardcode the computation *)
       let map_real_data =
         Map.of_alist_exn (module Int) [ 0, (0, 0); 1, (1, 1); 2, (2, 2) ]
       in
       let fake_assoc =
         Computation.Assoc
           { map_value = Return map_real_data
           ; key_witness = Int
           ; value_witness = Tuple (Int, Int)
           ; result_witness = Tuple (Int, Tuple (Int, Effect_func Int))
           ; f =
               (fun _key value ->
                  Subst
                    ( State { default_model = 1; default_witness = Int }
                    , Tuple (Int, Effect_func Int)
                    , fun state ->
                      Return
                        (Both
                           { first = Map (value, Tuple (Int, Int), Const (-1))
                           ; first_witness = Int
                           ; second = state
                           ; second_witness = Tuple (Int, Effect_func Int)
                           }) ))
           }
       in
       let fake_switch =
         Computation.Switch
           { either_value = Return (Second 1)
           ; first_witness = Unit
           ; second_witness = Int
           ; f_first = (fun _ -> fake_assoc)
           ; f_second = (fun _ -> fake_assoc)
           }
       in
       let packed : Computation.packed =
         T
           { unpacked = fake_switch
           ; witness = Map (Int, Tuple (Int, Tuple (Int, Effect_func Int)))
           }
       in
       (* Write the file *)
       let buffer = Buf.create 1024 in
       Buf.string buffer "open! Core";
       Buf.newline buffer;
       Buf.string buffer "open Bonsai.For_open";
       Buf.newline buffer;
       Buf.string buffer "open Bonsai.Let_syntax";
       Buf.newline buffer;
       Buf.newline buffer;
       Buf.string buffer "let computation = ";
       Buf.newline buffer;
       Buf.string buffer (packed_computation_to_ocaml_code ~indent:1 packed);
       Buf.newline buffer;
       Buf.string buffer "[@@warning \"-27\"]";
       Buf.newline buffer;
       Buf.string buffer ";;";
       print_endline (Buf.contents buffer)))
;;

let command =
  Command.group
    ~summary:"Generate random computations"
    [ "code", code_command; "hardcoded", hardcoded_command; "sexp", sexp_command ]
;;

let () = Command_unix.run command
