open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

module Id_gen (T : Int_intf.S) () = struct
  include T

  let component' ?(reset = (`Reset : [ `Reset | `Do_nothing | `Bump ])) (local_ graph) =
    let reset =
      match reset with
      | `Reset -> None
      | `Do_nothing -> Some (fun _ctx model -> model)
      | `Bump -> Some (fun _ctx model -> T.(model + one))
    in
    let model, fetch =
      Bonsai.actor
        ?reset
        ~sexp_of_model:[%sexp_of: T.t]
        ~equal:[%equal: T.t]
        ~sexp_of_action:[%sexp_of: Unit.t]
        ~default_model:T.minus_one
        ~recv:(fun _ctx i () ->
          let to_return = T.( + ) i T.one in
          to_return, to_return)
        graph
    in
    let fetch =
      let%arr fetch in
      fetch ()
    in
    fetch, model
  ;;

  let component ?(reset = (`Reset : [ `Reset | `Do_nothing ])) (local_ graph) =
    let fetch, _ = component' ~reset:(reset :> [ `Reset | `Do_nothing | `Bump ]) graph in
    fetch
  ;;
end
