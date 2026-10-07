# Plot Centrality

Plots one or more centrality measures, one facet per measure. Accepts
the data frame from
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) or
any network input.

## Usage

``` r
plot_centrality(
  x,
  measures = NULL,
  style = c("line", "bar", "lollipop", "dot"),
  orientation = c("horizontal", "vertical"),
  scale = c("raw", "normalized", "z", "rank"),
  order_by = NULL,
  top_n = NULL,
  highlight = 0L,
  cluster = NULL,
  palette = "cograph",
  ncol = NULL,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- x:

  Output of
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  or any network input (matrix, igraph, cograph_network, tna,
  netobject).

- measures:

  Character vector of measure names. When `x` is a network and
  `measures` is NULL (default), degree, strength, betweenness, closeness
  and eigenvector are computed. When `x` is a centrality data frame,
  NULL keeps all its measure columns.

- style:

  Character: "line" (default), "bar", "lollipop", or "dot".

- orientation:

  Character: "horizontal" (default, nodes on y-axis) or "vertical"
  (nodes on x-axis).

- scale:

  Character: "raw" (default, native units with a free value axis per
  facet), "normalized" (min-max scaled to \[0, 1\] within measure), "z"
  (standardized within measure), or "rank" (1 = highest value within
  measure).

- order_by:

  Character. Name of the measure column that sorts the nodes (e.g.,
  "degree_all"), or `"alpha"` for alphabetical order. Defaults to the
  first measure. In the "bar", "lollipop" and "dot" styles an unknown
  name raises an error; in the "line" style it falls back to the first
  measure.

- top_n:

  Optional integer. Keeps only the top-N nodes by `order_by` (by the
  first measure when `order_by = "alpha"`).

- highlight:

  Integer. In the "bar", "lollipop" and "dot" styles, the top-N nodes
  per measure are plotted in full color and the rest are muted. Default
  0 (no highlighting). Ignored by the "line" style.

- cluster:

  Optional cluster assignment of the nodes, given as the name of a
  column of `x`, a vector in node order, or a vector named by node.
  Colors the nodes by cluster in the "bar", "lollipop" and "dot" styles.

- palette:

  Currently unused. Cluster colors are taken from the built-in cograph
  palette.

- ncol:

  Number of facet columns. Default `NULL` uses up to three columns for
  eight or fewer measures and four otherwise.

- title:

  Plot title. Default NULL.

- subtitle:

  Plot subtitle. Default NULL.

- ...:

  Passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  when `x` is a network.

## Value

A ggplot object.

## Details

Four styles are available:

- `"line"`:

  Points connected by a line within each measure, with nodes in the
  order set by `order_by`.

- `"bar"`:

  Bars of the measure values.

- `"lollipop"`:

  Segments ending in a dot.

- `"dot"`:

  Dots only.

## Examples

``` r
plot_centrality(regulation_net)
```
