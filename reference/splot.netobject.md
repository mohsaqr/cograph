# Plot a Nestimate netobject

Applies TNA-compatible styling defaults before delegating to
[`splot()`](http://sonsoles.me/cograph/reference/splot.md): directed
networks get oval layout, coloured nodes, and sized arrows; undirected
networks get spring layout with no arrows or dashes. All parameters can
be overridden by the caller.

## Usage

``` r
splot.netobject(x, ...)
```

## Arguments

- x:

  A `netobject` (from Nestimate).

- ...:

  Additional arguments passed to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly returns the plot.
