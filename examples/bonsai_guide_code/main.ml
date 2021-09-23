open! Core
open! Bonsai_web

(* !!!TURN AWAY!!! *)
(* This is not good code!  It's made to interact in weird ways with docpub, and
   you shouldn't be looking here for inspiration.
   Of particular note:

   - The ui-components and call to "Bonsai.Start.start" are in the same file;
     they should be in separate files to encourage testability
   - Some code is written non-ideomatically because the ordering of the guide
     makes it undesierable to use concepts that weren't explained yet *)

module _ = Vdom_examples
module _ = Dynamism_examples
module _ = State_examples
module _ = Form_examples
module _ = Effect_examples
module _ = Flow_examples
module _ = Css_examples
module _ = Edge_examples
