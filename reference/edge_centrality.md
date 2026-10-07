# Calculate Edge Centrality Measures

Computes centrality measures for the edges of a network and returns a
tidy data frame with one row per edge.

## Usage

``` r
edge_centrality(
  x,
  measures = "all",
  weighted = TRUE,
  directed = NULL,
  cutoff = -1,
  invert_weights = NULL,
  alpha = 1,
  digits = NULL,
  sort_by = NULL,
  ...
)

edge_betweenness(x, ...)
```

## Arguments

- x:

  Network input (matrix, edge-list data frame, igraph, network,
  cograph_network, tna object).

- measures:

  Which measures to calculate. Default "all" calculates all available
  edge measures. Options: "betweenness", "weight", "overlap",
  "simmelian", "reciprocity".

- weighted:

  Logical. Use edge weights if available. Default TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- cutoff:

  Maximum path length for betweenness. Default -1 (no limit).

- invert_weights:

  Logical or NULL. Whether edge betweenness inverts the weights, so that
  higher weights mean shorter paths. The default `NULL` is TRUE for tna
  objects and FALSE otherwise.

- alpha:

  Numeric. Exponent of the inversion, which computes distances as
  `1 / weight^alpha`. Default 1.

- digits:

  Integer or NULL. Round numeric columns. Default NULL.

- sort_by:

  Character or NULL. Column to sort by (descending). Default NULL.

- ...:

  For `edge_centrality()`, the graph-construction arguments `loops` and
  `simplify` (see
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)).
  For `edge_betweenness()`, arguments passed to `edge_centrality()`.

## Value

`edge_centrality()` returns a base `data.frame` with one row per edge,
in the canonical (row-major) edge order of the input. The first two
columns are `from` and `to` (character when the input carried node
names, numeric indices otherwise); the remaining columns are those the
requested measures contribute, as listed in Details. `measures = "all"`
on an undirected input therefore gives `from`, `to`, `weight`,
`betweenness`, `overlap`, `shared_neighbors` and `triangles`, and a
directed input adds `reciprocated`, `reverse_weight` and `weight_ratio`.

`edge_betweenness()` returns a numeric vector of edge betweenness values
named `"from->to"`, for directed and undirected inputs alike.

## Details

Edge measures available, with the column(s) each one adds:

- betweenness:

  Edge betweenness, the sum over node pairs of the share of their
  shortest paths that pass through the edge. Adds `betweenness`.

- weight:

  Original edge weight (1 for an unweighted input). Adds `weight`.

- overlap:

  Jaccard neighborhood overlap of the edge endpoints. Adds `overlap` and
  the raw count `shared_neighbors`.

- simmelian:

  Number of triangles the edge participates in. Adds `triangles` (there
  is no column called `simmelian`).

- reciprocity:

  Whether the reverse edge exists. Directed only: on an undirected input
  it warns and adds nothing. Adds `reciprocated`, `reverse_weight` and
  `weight_ratio`, the last two `NA` where the edge is not reciprocated.

`measures = "all"` requests every measure, dropping `reciprocity` on an
undirected input.

## Examples

``` r
edge_centrality(regulation_net, measures = "betweenness")
#>          from         to betweenness
#> 1     Explore    Reflect         3.0
#> 2     Explore      Share        11.0
#> 3        Plan    Monitor         4.0
#> 4        Plan    Discuss         4.0
#> 5        Plan   Evaluate         7.0
#> 6        Plan     Create         7.5
#> 7        Plan      Share         2.0
#> 8     Monitor      Adapt        21.0
#> 9     Monitor     Create         6.0
#> 10      Adapt    Explore         3.0
#> 11      Adapt    Discuss         5.5
#> 12      Adapt Synthesize        15.5
#> 13    Reflect    Explore         6.0
#> 14    Reflect    Monitor        13.0
#> 15    Discuss    Explore         0.0
#> 16    Discuss    Reflect         1.0
#> 17    Discuss     Create         8.5
#> 18 Synthesize       Plan         9.5
#> 19 Synthesize    Monitor         3.0
#> 20 Synthesize    Reflect         3.0
#> 21   Evaluate    Monitor         0.0
#> 22   Evaluate      Adapt         0.0
#> 23   Evaluate    Reflect        12.0
#> 24     Create    Explore         5.0
#> 25     Create    Monitor         7.0
#> 26     Create   Evaluate         5.0
#> 27     Create      Share         5.0
#> 28      Share       Plan        15.0
#> 29      Share    Monitor         0.0
#> 30      Share      Adapt         3.0
```
