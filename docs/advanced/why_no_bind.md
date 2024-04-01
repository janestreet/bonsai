Why No Bind?

Many have asked the question "`Bonsai.t` is an Applicative, why isn't it
a Monad?" Certainly, for the users of a library, having a Monad gives
the user much more power. For example, take a look at this code using
monadic bind:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val x: bool t
val y: 'a t
val z: 'a t

val bind: 'a t -> f:('a -> 'b t) -> 'b t

bind x ~f:(fun x -> if x then y else z)
```

Look at that, we've built a `'a t` that dynamically chooses between
either `y` or `z` depending on the value of `x`. Try to do the same with
a type that is only an Applicative! (Do not try to do the same with an
Applicative, it is impossible.)

The dynamism on display is quite useful, but sadly, it's dynamism that
would hurt Bonsai if `Value.t` were a Monad. It would prevent
"whole-program analysis" of the app, which would make some nice Bonsai
features impossible:

1.  Bonsai runs an optimization pass on the program, resulting in a
    seriously condensed incremental graph. If an app were to dynamically
    generate bonsai values, we would potentially have to run the
    optimization many times a second, slowing things down considerably.
2.  We can attach instrumentation for debugging to nodes in the
    computation graph, or even add specialized "debugging" nodes.
3.  With `bind`-based dynamism, it would become difficult to reason
    about the "stateful" bits of Bonsai with models. If a `match%sub`
    node were to toggle between arms, state of the inactive subtrees
    would be lost.

And from a performance perspective, `bind` would likely make Bonsai apps
considerably slower: dynamism is fairly slow in Incremental, the library
that Bonsai is built on top of.

In practice, preventing Bonsai.t from implementing the Monad interface
hasn't blocked much: using the `match%sub` and `Bonsai.assoc`
primitives, pretty much anything you'd want to do with `bind` is
possible, and we get to keep all the benefits listed above.
