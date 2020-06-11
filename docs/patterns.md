# Bonsai Patterns
When using Bonsai, several patterns for organizing and modularizing code pop up
again and again.  This document is an attempt to record those patterns to make 
life easier for people that are new to Bonsai.

This list is nowhere near exhaustive; if you would like to see a pattern added,
feel free to contribute a new section!

<img style="border:0; display: block; margin: 0 auto;" src="./under_construction.gif" />

# `inject` function in `'result` position

A component may want to allow other components to raise its own events.  This
is done by returning the inject function from inside a component's `compute`
implementation.

