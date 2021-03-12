# Why no bind?

Many have asked the question "`Bonsai.Value.t` is an Applicative, why isn't it a 
Monad?"  Certainly, for the users of a library, having a Monad gives the user 
much more power.  For example, take a look at this code using monadic bind:

```ocaml
val x: bool t
val y: 'a t
val z: 'a t

val bind: 'a t -> f:('a -> 'b t) -> 'b t

bind x ~f:(fun x -> if x then y else z)
```

Look at that, we've built a `'a t` that dynamically chooses between either 
`y` or `z` depending on the value of `x`.  Try to do the same with a type that 
is only an Applicative! (Do not try to do the same with an Applicative, it is 
impossible.)

The dynamism on display is quite useful, but sadly, it's dynamism that would 
hurt Bonsai if `Bonsai.Value.t` were a Monad.  The main issue is that dynamism would 
prevent "whole-program analysis" of the app, and this would hurt a few neat features
of Bonsai:

1. Because we have "whole-program analysis," Bonsai runs an optimization pass on the
   program, resulting in a seriously condensed incremental graph.  If an app were 
   to dynamically generate bonsai values, we would potentially have to run the
   optimization many times a second, slowing things down considerably.
2. Another benefit (that we haven't taken advantage of yet) is that if we know
   about the whole program, we could insert "debug nodes" into the graph and
   present a "bonsai debugger" that shows the live values of any component's
   model or input.

Another issue that comes with dynamism is that it becomes difficult to reason
about the "stateful" bits of Bonsai with components that have models.  If a
dynamic node were to toggle between components, the models for the entire
dynamic sub-tree would be lost during the transition.  Going even further,
dynamism is fairly slow in Incremental, the library that Bonsai is built on top
of.

In practice, preventing Bonsai.t from implementing the Monad interface has
worked out pretty well.  Every time that someone has wanted a feature that they
could get via Monads, the Bonsai-dev team has found an alternative API that
solved their need.  For example `Bonsai.if_` is a great substitute for the code
provided at the top of this post.
