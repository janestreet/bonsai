open! Core
open Bonsai.For_open
open Bonsai.Let_syntax

let computation = 
  match%sub (Value.return (
    (Either.Second 1)
  ) ) with
  | First x146 -> (
    Bonsai.assoc 
      (module Int) 
      (
        Value.return (
          Map.of_alist_exn (module Int) [(0, (0, 0)); (1, (1, 1)); (2, (2, 2)); ]
        ) 
      ) ~f:(fun x159 x161 ->
        let%sub x168 = (
          Bonsai.state 
            ~default_model:1
            (module Int) 

        ) in
        return (
          Value.both 
            (Value.map (
              x161
            ) ~f:(fun _ -> -1))
            (x168)

        ) 
      ) 
  ) 
  | Second x152 -> (
    Bonsai.assoc 
      (module Int) 
      (
        Value.return (
          Map.of_alist_exn (module Int) [(0, (0, 0)); (1, (1, 1)); (2, (2, 2)); ]
        ) 
      ) ~f:(fun x173 x175 ->
        let%sub x182 = (
          Bonsai.state 
            ~default_model:1
            (module Int) 

        ) in
        return (
          Value.both 
            (Value.map (
              x175
            ) ~f:(fun _ -> -1))
            (x182)

        ) 
      ) 
  ) 
[@@warning "-27"]
;;
