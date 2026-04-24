# Plot Centrality

Publication-quality visualization of one or more centrality measures.
Accepts the data frame from
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
directly or any network input.

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

  Character vector of measure names. Default pulls the classical five
  (degree, strength, betweenness, closeness, eigenvector) when `x` is a
  network; default `NULL` keeps all columns when `x` is already a
  centrality data frame.

- style:

  Character: "line" (default), "bar", "lollipop".

- orientation:

  Character: "horizontal" (default, nodes on y-axis) or "vertical"
  (nodes on x-axis).

- scale:

  Character: "raw" (default, native units; in the "line" style this
  forces free y-axis per measure via faceting), "normalized" (\[0, 1\]
  within measure), "z" (standardized within measure), or "rank" (1..n,
  highest value = 1).

- order_by:

  Character. For "bar"/"lollipop": which measure sorts nodes. Defaults
  to the first measure. Use `"alpha"` for alphabetical. For "line", the
  x-axis always sorts nodes alphabetically so the parallel-coordinates
  paths are stable across plots.

- top_n:

  Optional integer to keep only the top-N nodes (by `order_by`). Useful
  for large graphs.

- highlight:

  Optional integer: highlight the top-N bars/lines per measure in full
  color; mute the rest. Default 0 (no highlighting).

- cluster:

  Optional named vector or data-frame column mapping each node to a
  cluster/community. Colors nodes by cluster when supplied.

- palette:

  Character or vector. `"cograph"` (default) uses cograph's
  teal-gold-leaf palette; `"okabe"` uses Okabe-Ito; `"viridis"` uses
  viridis; or supply a character vector of colors.

- ncol:

  For faceted styles ("bar", "lollipop"): number of columns. Default
  `NULL` chooses sensibly based on measure count.

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

Three styles are available:

- `"line"`:

  Parallel-coordinates view. Nodes on the x-axis, z-score on the y-axis,
  one polyline per measure. Best for spotting nodes with atypical
  centrality profiles.

- `"bar"`:

  Horizontal bars, one facet per measure. Best for reading individual
  measure values.

- `"lollipop"`:

  Like `"bar"` but with a dot at the tip. Softer visual weight; useful
  on dense grids.

## Examples

``` r
adj <- matrix(c(0,1,1,0,0, 1,0,1,1,0, 1,1,0,1,1, 0,1,1,0,1, 0,0,1,1,0),
              5, 5)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
plot_centrality(adj)

plot_centrality(adj, style = "bar", highlight = 2)
```
