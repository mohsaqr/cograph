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

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A data frame with columns `from`, `to`, and one column per requested
measure.

Named numeric vector of edge betweenness values (named by `"from->to"`).

## Details

Edge measures available:

- betweenness:

  Number of shortest paths passing through the edge.

- weight:

  Original edge weight.

- overlap:

  Jaccard neighborhood overlap of edge endpoints.

- simmelian:

  Number of triangles the edge participates in.

- reciprocity:

  Whether the reverse edge exists (directed only). Adds columns:
  `reciprocated`, `reverse_weight`, `weight_ratio`.

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
