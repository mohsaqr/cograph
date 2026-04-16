# Compute Shortest Path Distances

Computes shortest path distances between nodes in a network. Supports
all-pairs, single-source, and point-to-point queries.

## Usage

``` r
shortest_paths(x, from = NULL, to = NULL, weights = NULL, directed = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- from:

  Character or numeric node identifier(s) for the source. If NULL
  (default), compute distances from all nodes.

- to:

  Character or numeric node identifier(s) for the target. If NULL
  (default), compute distances to all nodes.

- weights:

  Edge weight handling: NULL (default) auto-detects from edge
  attributes, NA forces unweighted distances, or a numeric vector of
  custom weights.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Depends on the query:

- If both `from` and `to` are NULL: a full distance matrix (all pairs)

- If `from` is a single node and `to` is NULL: a named numeric vector of
  distances from that node to all others

- If `from` is multiple nodes and `to` is NULL: a matrix with rows for
  each source

- If both `from` and `to` are single nodes: a single numeric value

- Otherwise: a matrix of distances between the specified node sets

## Details

Uses
[`igraph::distances()`](https://r.igraph.org/reference/distances.html)
internally. For weighted networks, edge weights are used as distances by
default. Pass `weights = NA` to ignore weights and treat all edges as
having unit distance.

Note:
[`igraph::distances()`](https://r.igraph.org/reference/distances.html)
with `weights = NULL` automatically uses edge weight attributes if
present. To force unweighted computation, pass `weights = NA`
explicitly.

## See also

[`k_shortest_paths`](https://sonsoles.me/cograph/reference/k_shortest_paths.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
# All-pairs distances
adj <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), 4, 4)
rownames(adj) <- colnames(adj) <- LETTERS[1:4]
cograph::shortest_paths(adj)
#>   A B C D
#> A 0 1 2 3
#> B 1 0 1 2
#> C 2 1 0 1
#> D 3 2 1 0

# Single source to all
cograph::shortest_paths(adj, from = "A")
#> A B C D 
#> 0 1 2 3 

# Point-to-point
cograph::shortest_paths(adj, from = "A", to = "D")
#> [1] 3
```
