# Bonsai Beyond the Web

So far, we've talked about Bonsai solely as a library for building scalable
and composable web UIs with OCaml. It may be surprising to know
that Bonsai isn't specialized for user interfaces; rather, it answers
the very generic question of how to build composable incremental
state-machines. As it turns out, incremental state-machines are a great
abstraction for building UI!

Bonsai computations can produce anything; even in our web UIs, we've used
plenty of `int Computation.t`, `string Computation.t`, and others. Unlike 
React, computations around state and logic are first-class, just like those
around vdom.

<!-- 
I think the VR and Wall integrations are definitely worth mentioning here,
but are there other non-UI examples we could share?
-->