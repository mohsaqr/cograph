# Plot a motif/subgraph result

Tab-completion-friendly wrapper around the `plot.cograph_motif_result`
S3 method. Functionally identical to `plot(x, ...)` on a
`cograph_motif_result` object, but exposes the
`type / n / ncol / colors` arguments to editor autocompletion.

## Usage

``` r
plot_motifs(
  x,
  type = c("triads", "types", "significance", "patterns"),
  n = 15,
  ncol = 5,
  colors = c("#2166AC", "#B2182B"),
  ...
)
```

## Arguments

- x:

  Input data: a tna object, cograph_network, matrix, igraph, or
  data.frame (edge list).

- type:

  Plot type:

  `"triads"`

  :   Network diagrams of specific node triples (instance mode) or falls
      back to patterns (census mode). Arranged in a grid.

  `"types"`

  :   Bar chart of MAN type frequencies.

  `"significance"`

  :   Z-score plot showing over- and under-represented types relative to
      a null model. Requires `significance = TRUE` in the
      [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md)
      call.

  `"patterns"`

  :   Abstract MAN pattern diagrams showing the edge structure of each
      triad type.

- n:

  Maximum number of items to plot. Default 15.

- ncol:

  Number of columns in the triad/pattern grid. Default 5.

- colors:

  Two-element color vector: first color for over-represented or positive
  values, second for under-represented or negative values. Default
  `c("#2166AC", "#B2182B")` (blue/red).

- ...:

  Additional arguments passed to internal plot helpers.

## Value

Invisibly returns the input `x` (or the underlying `ggplot` for the
`"types"` and `"significance"` types, matching the S3 method).

## See also

[`motifs`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs`](https://sonsoles.me/cograph/reference/subgraphs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- igraph::sample_gnp(20, 0.2, directed = TRUE)
m <- motifs(g)
plot_motifs(m)
plot_motifs(m, type = "types")
} # }
```
