# Sharing State

<!-- I'm not sure if we want a section on this, so for now
I've pretty much just copied what I wrote for my blog.
If we want to keep this, I'd like to clean it up a bit,
and come up with better use cases for global and dynamic scoped
state.

I'd also interleave the general introductions with the Bonsai
explanations + examples.  -->

In [chapter 3](03-state.md), we discussed some of Bonsai's primitives for
storing and updating state within a single component. 

Most non-trivial UIs will need to share state between components.
There's a few patterns we can use, depending on what exactly we want to achieve.

If state is shared between several components that are close together,
we can store state in the "closest common ancestor" component,
and pass it down through inputs to child components.
See [React's Lifting State Up article](https://reactjs.org/docs/lifting-state-up.html) for examples.

Sometimes, many components depend on some shared state/environment.
For example, let's say we have a UI that can take on one of several themes.
We don't want to hardcode a particular theme, so that users can change themes at will.
We also don't want to explicitly pass the current theme as an argument to **every** component,
since this would get messy fast. We could put it in a global variable (more on this later),
but what if we want to build a "theme preview" system that shows the same component tree twice
with different themes, side by side?

For this case, we want to use something called **Context**, **Dynamic Scope,** or **Environment**, where a root component declares
values for some semi-global state, which is then accessible to all the root's subcomponents.
For more on this type of state-sharing, read about [React's Context](https://reactjs.org/docs/context.html).

In other cases, we might have a global bit of state that is
read/written to by two components that are very far apart.
For example, we could have switches in a sidebar that controls
some small part of a separate dashboard. We don't want to lift this state up,
since it would need to be passed through many unrelated components.
It also doesn't qualify as context, since it's only applicable to a few components.
In this case, we'll want to put that state in a global variable,
which will only be used by the related components.
To be explicit about the behavior of this global variable,
we could put it behind a model/action/apply_action pattern like
the one provided by `Bonsai.state_machine`
except that it would be available globally instead of in a single component.

## Bonsai's Tools

How can we implement these paradigms in Bonsai?

"Lifting state up" is the simplest: a parent component can just
pass state data/setters (wrapped in `Value.t`) to inner components as arguments when evaluating them.

"Context" is done through the `Bonsai.Dynamic_scope` module.
You can create an "environment" variable using `Dynamic_scope.create`,
and access its value as a `Computation.t` with `Dynamic_scope.lookup`.
Then, to evaluate a component tree with some value for a `Dynamic_scope` variable,
you can call `Dynamic_scope.set env_var value ~inside:(tree_to_evaluate)`,
which will return a new computation where all uses of `Dynamic_scope.lookup env_var`
will return the set value.

Global variables are a bit tricky. At first, you might think that you could
just create a `Bonsai.state` and use it in several components,
but recall that each `let%sub` instantiation of a `Computation.t`
creates a new, unrelated instance. Instead, we can use a `Bonsai.Var`
in the global scope to create a mutable variable that can be shared between components.
If you want your components to re-render when the `Bonsai.Var.t` changes,
make sure to access it via `Bonsai.Var.value var_instance`.

Because Bonsai computations can return anything, not just vdom, we can also
build components that return a view *and* some data, like we did when implementing
textbox in the [state docs](03-state.md). This pattern is useful when only one component can
write state, and one or more siblings need to read it. Although of course, you could return
the `set_state` (or `inject` function) too.