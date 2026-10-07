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

  Not used. Any argument supplied here raises an `"unused argument"`
  error.

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

Distances are computed with
[`igraph::distances()`](https://r.igraph.org/reference/distances.html).
For weighted networks, edge weights are used as distances by default.
With `weights = NA`, every edge has unit distance.

igraph also exports a `shortest_paths()` with a different signature and
return value; when both packages are attached, qualify the call as
`cograph::shortest_paths()`.

## See also

[`k_shortest_paths`](https://sonsoles.me/cograph/reference/k_shortest_paths.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
cograph::shortest_paths(regulation_net, from = "Plan")
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       0.34       0.00       0.13       0.29       0.56       0.40       0.46 
#>   Evaluate     Create      Share 
#>       0.49       0.20       0.36 
```
