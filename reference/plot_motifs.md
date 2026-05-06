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
  node_size = 5,
  label_size = 11,
  title_size = 12,
  stats_size = 13,
  legend_size = 13,
  legend = TRUE,
  motif_color = "#800020",
  spacing = 1,
  base_size = 12,
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

- node_size:

  Triad node radius (relative). Default 5. (`type = "triads"` only.)

- label_size:

  Triad node-label font size in points. Default 11.

- title_size:

  Per-panel title font size in points. Default 12.

- stats_size:

  Per-panel statistics caption font size in points (e.g.,
  `n=34 z=-55.3 p<.001`). Default 13.

- legend_size:

  Bottom legend font size in points. Default 13.

- legend:

  Logical. Show the abbreviation legend strip below the triad grid.
  Default `TRUE`. (`type = "triads"` only.)

- motif_color:

  Color of triad nodes/edges/labels. Default `"#800020"` (deep
  burgundy). (`type = "triads"` only.)

- spacing:

  Triangle spread inside each panel; `> 1` pulls nodes inward, `< 1`
  pushes them apart. Default 1.

- base_size:

  Base font size for the `ggplot2` themes used by `type = "types"` and
  `type = "significance"`. Default 12.

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
