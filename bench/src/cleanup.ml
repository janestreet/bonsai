open! Core
open! Bonsai_test.Arrow

type packed = T : _ Driver.t -> packed

let (most_recent_driver : packed option ref) = ref None
let register_driver driver = most_recent_driver := Some (T driver)

let invalidate_observers =
  Core_bench_js.Test.create_with_initialization
    ~name:"cleaning up observers..."
    (fun `init ->
       (match !most_recent_driver with
        | None -> ()
        | Some (T driver) -> Driver.invalidate_observers driver);
       most_recent_driver := None;
       fun () -> ())
;;
