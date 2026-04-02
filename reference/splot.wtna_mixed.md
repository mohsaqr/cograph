# Plot a Mixed Window TNA Object

Plot a `wtna_mixed` object either as a single overlaid network or as two
separate group panels.

## Usage

``` r
splot.wtna_mixed(x, type = c("overlay", "group"), ...)
```

## Arguments

- x:

  A `wtna_mixed` object (from Nestimate `wtna(..., method = "both")`).

- type:

  Character. `"overlay"` (default) renders both networks on a single
  canvas via
  [`plot_mixed_network`](http://sonsoles.me/cograph/reference/plot_mixed_network.md)
  — co-occurrence as straight undirected edges, transitions as curved
  directed arrows. `"group"` plots each component as a separate panel.

- ...:

  Additional arguments passed to
  [`plot_mixed_network`](http://sonsoles.me/cograph/reference/plot_mixed_network.md)
  (`type = "overlay"`) or
  [`splot`](http://sonsoles.me/cograph/reference/splot.md)
  (`type = "group"`).

## Value

Invisibly returns `x`.
