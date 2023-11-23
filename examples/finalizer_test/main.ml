open! Core
open Js_of_ocaml

module type Test = sig
  val name : string
  val count : unit -> int
end

let heap_block () = Heap_block.create_exn (ref 0)

module Add_finalizer_last = struct
  let on_garbage_collection, name = [%demo Gc.Expert.add_finalizer_last_exn]
  let count = ref 0

  let (_ : _) =
    let f () =
      let x = heap_block () in
      incr count;
      on_garbage_collection x (fun () -> decr count)
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 1.0
  ;;

  let count () = !count
end

module Add_finalizer = struct
  let count = ref 0
  let on_garbage_collection, name = [%demo Gc.Expert.add_finalizer_exn]

  let (_ : _) =
    let f () =
      let x = heap_block () in
      incr count;
      on_garbage_collection x (fun _ -> decr count)
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 1.0
  ;;

  let count () = !count
end

module Weak = struct
  let all = Weak.create 1000
  let name = "Weak.t"

  let (_ : _) =
    let f () =
      let written = ref false in
      for i = 0 to Weak.length all - 1 do
        if (not (Weak.check all i)) && not !written
        then (
          Weak.set all i (Some (heap_block ()));
          written := true)
      done
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 1.0
  ;;

  let count () =
    let count = ref 0 in
    for i = 0 to Weak.length all - 1 do
      if Weak.check all i then incr count
    done;
    !count
  ;;
end

module Weak_pointer = struct
  let all = Array.init 1000 ~f:(fun _ -> Weak_pointer.create ())
  let name = "Weak_pointer.t"

  let (_ : _) =
    let f () =
      let written = ref false in
      for i = 0 to Array.length all - 1 do
        if Weak_pointer.is_none (Array.get all i) && not !written
        then (
          Array.set all i (Weak_pointer.create_full (heap_block ()));
          written := true)
      done
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 1.0
  ;;

  let count () =
    let count = ref 0 in
    for i = 0 to Array.length all - 1 do
      if Weak_pointer.is_some (Array.get all i) then incr count
    done;
    !count
  ;;
end

module Weak_array = struct
  let all = Weak_array.create ~len:1000
  let name = "Weak_array.t"

  let (_ : _) =
    let f () =
      let written = ref false in
      for i = 0 to Weak_array.length all - 1 do
        if (not (Weak_array.is_some all i)) && not !written
        then (
          Weak_array.set all i (Some (heap_block ()));
          written := true)
      done
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 1.0
  ;;

  let count () =
    let count = ref 0 in
    for i = 0 to Weak_array.length all - 1 do
      if Weak_array.is_some all i then incr count
    done;
    !count
  ;;
end

module Weak_hashtbl = struct
  let name = "Weak_hashtbl.t"
  let counter = ref 0
  let should_reclaim = ref false
  let all = Weak_hashtbl.create (module Int)

  let () =
    Weak_hashtbl.set_run_when_unused_data all ~thread_safe_f:(fun () ->
      should_reclaim := true)
  ;;

  let (_ : _) =
    let f () =
      incr counter;
      Weak_hashtbl.add_exn all ~key:!counter ~data:(heap_block ())
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 1.0
  ;;

  let count () =
    if !should_reclaim
    then (
      Weak_hashtbl.reclaim_space_for_keys_with_unused_data all;
      should_reclaim := false);
    let count = ref 0 in
    for i = 0 to !counter do
      if Weak_hashtbl.key_is_using_space all i then incr count
    done;
    !count
  ;;
end

let () =
  let tests =
    ([ (module Add_finalizer_last)
     ; (module Weak)
     ; (module Weak_pointer)
     ; (module Weak_array)
     ; (module Weak_hashtbl)
     ; (module Add_finalizer)
     ]
      : (module Test) list)
  in
  let (_ : _) =
    let f () =
      let s =
        List.map tests ~f:(fun (module T) ->
          [%string "<tr><td>%{T.name}</td><td>%{(Int.to_string (T.count ()))}</td></tr>"])
        |> String.concat ~sep:"\n"
        |> fun s ->
        [%string
          {| <div> For solutions that work properly, the numbers in the second column should reset to 0 when a major GC collection occurs. </div>
             <table>%{s}</table>|}]
        |> Js.string
      in
      Dom_html.document##.body##.innerHTML := s
    in
    Dom_html.window##setInterval (Js.wrap_callback f) 16.0
  in
  ()
;;
