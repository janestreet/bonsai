# Dygraph

Bindings to dygraphs, a javascript graphing library, using gen_js_api.

    Dygraphs: http://dygraphs.com/
    API: http://dygraphs.com/jsdoc/symbols/Dygraph.html
    Options Reference: http://dygraphs.com/options.html

## High level overview

- Graph: A dygraph.  This API is actually surprisingly small since
  almost all options live in...

- Options: Where you can control almost any aspect of the graph.  Each
  option should be documented, which is copied from
  http://dygraphs.com/options.html.

- Data: Dygraphs can take data of a few formats (the x-values can be
  floats, dates, or times).  This module provides nice [create]
  functions for each of these options.

- With_bonsai: **The recommended way to use a dygraph (with bonsai).**

## Sharp corners

### CSP (content security policy)

The default content security policy for simple web server does not
allow unsafe inline styles, even from self.  Unfortunately, dygraphs
does this.  So in order to not have error messages in your console
when using dygraphs, you should use
`Content_security_policy.default_for_clientside_rendering_internal` in
your web server.

## Examples

See lib/dygraph/examples for a few ocaml and javascript examples.
`Stock chart` in particular is good because we have both the ocaml and
javascript example so that you can compare.

## References:

- http://dygraphs.com/
- http://dygraphs.com/jsdoc/symbols/Dygraph.html
- http://dygraphs.com/options.html.
- http://dygraphs.com/data.html
- http://dygraphs.com/gallery
