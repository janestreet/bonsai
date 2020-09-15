"Bonsai!"
=========

Bonsai is a library that is used to build browser-based frontend applications
in OCaml.  It builds on top of the technology and lessons learned from
`Incr_dom` [^incr_dom], an incremental frontend framework.

# Getting Started

The [Getting Started with Bonsai](./docs/getting_started/index.md)
guide is good if you're new to web development entirely or just want
to see a walkthrough of a couple simple example apps.

Examples of using Bonsai in a web browser can be found in the
`examples` directory.

# Differences between Incr_dom and Bonsai

The main differences between `Incr_dom` and Bonsai are

- Bonsai has a notion of first-class components
- `Incr_dom` expects users to program in the `Incremental` monad; in Bonsai,
  incrementality is added for you under the hood -
  [read more](./docs/incrementality.md)
- Combining Bonsai components is easy

Similarities between `Incr_dom` and Bonsai:

- Both are built on top of Incremental library for performance optimizations
- Both make use of the `Vdom` library and associated helper libraries
  (`lib/vdom_input_widgets`, `lib/vdom_keyboard`, etc..) to help developers
  construct the view of the app.
- Bonsai components are embeddable inside of existing `Incr_dom` apps!

# Bonsai Documentation Table of Contents

<!-- This table-of-contents is very dependent on the exact whitespace present.
Pandoc really wants to stick paragraph <p> nodes any time that there's even a trace of
whitespace in between these html nodes... -->
<div class="toc_node"> <a href="./README.md"> [This Page] </a>
<div class="toc_node"> <a href="./docs/history.md"> A History of Bonsai </a></div>
<div class="toc_node"> <a href="./docs/incrementality.md"> Incrementality in Bonsai </a></div>
<div class="toc_node"> <a href="./docs/inside_incr_dom.md"> Using Bonsai inside an Incr_dom App </a></div>
<div class="toc_node"> <a href="./docs/bonsai_web.md"> Bonsai_web </a></div>
</div>
</div>

[^incr_dom]:
  For interoperability between Bonsai and `Incr_dom`, see [Using Bonsai Inside
  Incr_dom](./docs/inside_incr_dom.md).
  For a short history of Bonsai and `Incr_dom`, see [History](./docs/history.md)
