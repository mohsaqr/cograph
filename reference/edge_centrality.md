# Calculate Edge Centrality Measures

Computes centrality measures for edges in a network and returns a tidy
data frame. Unlike node centrality, these measures describe edge
importance.

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

  Network input (matrix, igraph, network, cograph_network, tna object)

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

  Logical or NULL. Invert weights for path-based measures? Default NULL
  (auto-detect: TRUE for tna objects, FALSE otherwise).

- alpha:

  Numeric. Exponent for weight inversion. Default 1.

- digits:

  Integer or NULL. Round numeric columns. Default NULL.

- sort_by:

  Character or NULL. Column to sort by (descending). Default NULL.

- ...:

  Additional arguments forwarded to the graph constructor, namely
  `loops` and `simplify` (see
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)).

## Value

A base `data.frame` with one row per edge, in the canonical (row-major)
edge order of the input. The first two columns are `from` and `to`
(character when the input carried node names, numeric indices
otherwise); the remaining columns are those the requested measures
contribute, as listed in Details. `measures = "all"` on an undirected
input therefore gives `from`, `to`, `weight`, `betweenness`, `overlap`,
`shared_neighbors` and `triangles`, and a directed input adds
`reciprocated`, `reverse_weight` and `weight_ratio`.

Named numeric vector of edge betweenness values (named by `"from->to"`).

## Details

Edge measures available, with the column(s) each one adds:

- betweenness:

  Number of shortest paths passing through the edge. Adds `betweenness`.

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
# Create test network
mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,0, 0,1,0,0), 4, 4)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

# All edge measures
edge_centrality(mat)
#>   from to weight betweenness overlap shared_neighbors triangles
#> 1    A  B      1           2     0.5                1         1
#> 2    A  C      1           1     1.0                1         1
#> 3    B  C      1           2     0.5                1         1
#> 4    B  D      1           3     0.0                0         0

# Just betweenness
edge_centrality(mat, measures = "betweenness")
#>   from to betweenness
#> 1    A  B           2
#> 2    A  C           1
#> 3    B  C           2
#> 4    B  D           3

# Sort by betweenness to find bridge edges
edge_centrality(mat, sort_by = "betweenness")
#>   from to weight betweenness overlap shared_neighbors triangles
#> 1    B  D      1           3     0.0                0         0
#> 2    A  B      1           2     0.5                1         1
#> 3    B  C      1           2     0.5                1         1
#> 4    A  C      1           1     1.0                1         1
mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,0, 0,1,0,0), 4, 4)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
edge_betweenness(mat)
#> A->B A->C B->C B->D 
#>    2    1    2    3 
```
