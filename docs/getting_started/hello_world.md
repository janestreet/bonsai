# Building the "Hello world" Bonsai app

Most OCaml programs result in an .exe file that you run via the command
line. But Bonsai uses
[js_of_ocaml](https://github.com/ocsigen/js_of_ocaml) to produce a
JavaScript file with the extension `.bc.js` (the "bc" for "bytecode")
that is then included in an HTML page.

To see this in action, let's walk through the process of running the
simple example in ../../examples/hello_world.

## Step 1: The build

Follow the Dune instructions for [building javascript
executables](https://dune.readthedocs.io/en/stable/jsoo.html)

Once you mark a library or executable with `js_of_ocaml`, you are in
effect declaring that your only dependencies will also be
`js_of_ocaml`-compatible libraries. In particular, you're disallowed
from using libraries that won't work in a Javascript runtime, like
Core_unix or Async -- instead, you have to use their non-Unix versions
Core and Async_kernel.

## Step 2: The HTML scaffold

Remember that Bonsai emits a Javascript file with your app's logic. To
get this Javascript to run, we have to include it on the HTML page that
will ultimately be served to your browser. We do that by putting a
`<script>` tag in the HTML page's source:

```{=html}
<!-- If the contents of this change, consider updating the doc! -->
```
``` sh
$ cat ../../examples/hello_world/index.html
<!DOCTYPE html>
<html>
    <head>
        <meta charset="UTF-8">
        <title>Hello, Bonsai!</title>
        <script defer src="main.bc.js"></script>
    </head>
    <body>
        <div id="app"></div>
    </body>
</html>
```

Notice that "app" `<div>`. That's where Bonsai will "attach" itself,
i.e., that is the element that you will be putting under Bonsai's
control. You'll see in a minute how this is wired up.

## Step 3: Start the server

For this example you can start any old HTML server. Here's an easy way:

`sh skip $ cd lib/bonsai/examples/hello_world; python -m SimpleHTTPServer`

The command will pick a random available port and print out a link you
should to navigate to in your browser.

**Note:** Once you start the server, you don't need to restart it
whenever you make changes. Instead, the javascript file will be
recompiled by Jenga, and when you reload the web page, the browser
fetches the new copy.

## Step 4: Walking through the code

How do the words "hello world" get on the page? Let's look at the code.

```{=html}
<!-- If the contents of this change, consider updating the doc! -->
```
``` sh
$ cat ../../examples/hello_world/main.ml
open! Core
open! Bonsai_web

let component = Bonsai.const (Vdom.Node.text "hello world")
let () = Bonsai_web.Start.start component
```

The component is a `Vdom.Node.t Bonsai_web.Computation.t` constructed
with the simplest possible constructor, `Bonsai.const`, which simply
returns as a result whatever it's passed.

By default, `start` binds to the to the element with id "app" in our
HTML page.

A Vdom node could contain a deeply nested tree full of elements and
callbacks; but in this case, it's just the single text node with the
contents "hello world". (Just about every branch of the DOM on any web
page bottoms out in a text node or image.)

That's it!
