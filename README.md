"Bonsai!"
=========

Bonsai is a library that is used to build browser-based frontend applications
in OCaml.  It builds on top of the technology and lessons learned from
`Incr_dom` [^incr_dom], an incremental frontend framework.

The main differences between `Incr_dom` and Bonsai are

- Bonsai has a notion of first-class components -
  [read more](./docs/concepts.md)
- `Incr_dom` expects users to program in the `Incremental` monad; in Bonsai,
  incrementality is added for you under the hood -
  [read more](./docs/incrementality.md)
- Combining Bonsai components is easy (and fun!) -
  [read more](./docs/combinators.mdx)

Similarities between `Incr_dom` and Bonsai:

- Both are built on top of Incremental library for performance optimizations
- Both make use of the `Vdom` library and associated helper libraries
  (`lib/vdom_input_widgets`, `lib/vdom_keyboard`, etc..) to help developers
  construct the view of the app.
- Bonsai components are embeddable inside of existing `Incr_dom` apps!

# Table of Contents

<!-- This table-of-contents is very dependent on the exact whitespace present.
Pandoc really wants to stick paragraph <p> nodes any time that there's even a trace of
whitespace inbetween these html nodes... -->
<div class="toc_node"> <a href="./README.md"> [This Page] </a>
<div class="toc_node"> <a href="./docs/concepts.md"> Bonsai Concepts </a></div>
<div class="toc_node"> <a href="./docs/history.md"> A History of Bonsai </a></div>
<div class="toc_node"> <a href="./docs/incrementality.md"> Incrementality in Bonsai </a></div>
<div class="toc_node"> <a href="./docs/inside_incr_dom.md"> Using Bonsai inside an Incr_dom App </a></div>
<div class="toc_node"> <a href="./docs/bonsai_web.md"> Bonsai_web </a></div>
<div class="toc_node"> <a href="./docs/api_tour.mdx"> The Bonsai API </a>
<div class="toc_node"> <a href="./docs/constructors.mdx">Constructors </a> </div>
<div class="toc_node"> <a href="./docs/combinators.mdx"> Combinators</a></div>
<div class="toc_node"> <a href="./docs/projections.mdx"> Projections </a></div>
<div class="toc_node"> <a href="./docs/collections.mdx"> Collections </a></div>
</div>
</div>

# Getting Started

Reading [Bonsai Concepts](./docs/concepts.md) is the best place to get an
overview of the library and to build a mental model for structuring Bonsai
applications.

Examples of using Bonsai in a web browser can be found in the
`examples` directory.

[^incr_dom]:
  For interoperability between Bonsai and `Incr_dom`, see [Using Bonsai Inside
  Incr_dom](./docs/inside_incr_dom.md).
  For a short history of Bonsai and `Incr_dom`, see [History](./docs/history.md)
