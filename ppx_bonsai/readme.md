# ppx_bonsai

A ppx rewriter for Bonsai's %sub and %arr let syntaxes.

let%sub
-------

`let%sub` is a form almost equivalent to `let%bind` but calling a function
called [sub] instead of [bind]. The intended use case is for things which have
a "bind-like" operation with a type like:

Unlike `let%map` and `let%bind`, `let%sub` does _not_ permit
multiple bindings via the `and` keyword.

```ocaml
val sub : 'a t -> f:('a s -> 'b t) -> 'b t
```

(e.g. a relative monad) The name comes from the quintessential example
of such an operation: substitution of terms for variables.  We didn't
want to just use [let%bind] for such functions as it might confuse
people.

There is one large difference between `let%sub` and `let%bind` stemming from
the difference in the expected signatures of `sub` and `bind`. Since the value
passed into `f` is not totally "unwrapped", it cannot be directly destructured.
Because accessing the components of complex structures is often desirable,
`let%sub` does the extra work. The below code snippet

```ocaml
let%sub a, b = c in
BODY
```

gets roughly translated to

```ocaml
let%sub temp_var = c in
let%sub a = return (map ~f:(fun (a, _) -> a) temp_var) in
let%sub b = return (map ~f:(fun (_, b) -> a) temp_var) in
BODY
```

The one potentially unexpected part of this is the usage of `return` followed
immediately by `let%sub`, which seems like a no-op. Why not do this instead?

```ocaml
let%sub temp_var = c in
let a = map ~f:(fun (a, _) -> a) temp_var in
let b = map ~f:(fun (_, b) -> a) temp_var in
BODY
```

The difference is that the second option binds `a` and `b` to the
*computations* that map `temp_var` to its components, but the first option
binds `a` and `b` to the components *after the mappings have occurred*.
Conceptually this means that for the first, correct desugaring, reusing `a` and
`b` does not duplicate the mapping computation, but for the second desugaring, every
usage of `a` or `b` refers to a duplicate of its computation.

match%sub
---------

Rather than depending on a `bind` operation, `match%sub` depends on the
presence of a `switch` function (the concrete Bonsai version is shown below).
Note that the type of the expression being matched is `Value.t` rather than
`Computation.t`.

```ocaml
val switch
  :  match_:int Value.t
  -> branches:int
  -> with_:(int -> 'a Computation.t)
  -> 'a Computation.t
```

Example:

```ocaml
let f (either_value : (_, _) Either.t Value.t) page1 page2 =
  let open Bonsai.Let_syntax in
  match%sub either_value with
  | First (a, b) -> page1 a b
  | Second x -> page2 x
;;
```

expands to roughly the following

```ocaml
let f (either_value : (_, _) Either.t Value.t) page1 page2 =
  let open Bonsai.Let_syntax in
  let%sub either_value = return either_value in
  Let_syntax.switch
    ~match_:
      (match%map either_value with
       | First (_, _) -> 0
       | Second _ -> 1)
    ~branches:2
    ~with_:(function
      | 0 ->
        let%sub a =
          return
            (match%map either_value with
             | First (a, _) -> a
             | _ -> assert false)
        in
        let%sub b =
          return
            (match%map either_value with
             | First (_, b) -> b
             | _ -> assert false)
        in
        page1 a b
      | 1 ->
        let%sub x =
          Bonsai.Let_syntax.return
            (match%map either_value with
             | Second x -> x
             | _ -> assert false)
        in
        page2 x
      | _ -> assert false)
;;
```

let%arr
-------

One way of looking at arrow programs (i.e. programs involving
`let%sub`) is that `let%map...and` builds a computation that does
work, and `let%sub` saves the work to a variable so that other
computations can share the result.

A common mistake is to forget to save a computation; when the
computation gets used twice, the work must be done twice, rather than
getting shared between the two occurrences. The form `let%arr...and`
aims to eliminate such mistakes via the type-system. It extends the
`Let_syntax` module with an `arr` function that lifts a function to an
arrow.

```ocaml
val arr : 'a Value.t -> f:('a -> 'b) -> 'b Computation.t
```

The signature is the same as `map` except the result is
a `Computation.t`, and not a `Value.t`. The implementation of `arr` is
equivalent to `return (map x ~f)`. Roughly the only thing you can do
with a `Computation.t` is use `let%sub` to gain access to a `Value.t`
handle to the computation. Thus, using `arr` forces the user to use as
much sharing as possible. Of course, you can always duplicate work by
copy-pasting code, but it won't happen accidentally.

An unrelated distinction of `let%arr` is that it tracks
`Lexing.position`. That is, the signature of `arr` is actually the
following:

```ocaml
val arr : ?here:Lexing.position -> 'a Value.t -> f:('a -> 'b) -> 'b Computation.t
```

The `let%arr` form is as a testing ground for source location
tracking. It's likely that eventually all the other binding forms will
also track location.

