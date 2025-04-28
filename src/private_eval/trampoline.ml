open! Core

type 'a t =
  | Lazy : 'a t Lazy.t -> 'a t
  | Return : 'a -> 'a t
  | Bind : 'a t * ('a -> 'b t) -> 'b t

let lazy_ t = Lazy t
let return a = Return a
let bind t ~f = Bind (t, f)

type (_, _) stack =
  | [] : ('a, 'a) stack
  | ( :: ) : ('a -> 'b t) * ('b, 'c) stack -> ('a, 'c) stack

let rec run_aux : type a b. a t -> (a, b) stack -> b =
  fun t stack ->
  match t with
  | Lazy t -> run_aux (Lazy.force t) stack
  | Bind (t, k) -> run_aux t (k :: stack)
  | Return a ->
    (match stack with
     | [] -> a
     | k :: stack -> run_aux (k a) stack)
;;

let run t = run_aux t []

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let bind = bind
    let return = return
    let map = `Define_using_bind
  end)

let all_map (type k v) (map : (k, v t, 'cmp) Map.t) =
  map
  |> Map.to_alist
  |> List.map ~f:(fun (k, v) -> bind v ~f:(fun v -> return (k, v)))
  |> all
  |> bind ~f:(fun alist -> return (Map.of_alist_exn (Map.comparator_s map) alist))
;;

let both a b = bind a ~f:(fun a -> bind b ~f:(fun b -> return (a, b)))

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    let both = both
  end
end
