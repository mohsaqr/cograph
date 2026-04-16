# Find K Shortest Loopless Paths (Yen's Algorithm)

Computes up to `k` shortest loopless paths between two nodes using Yen's
algorithm. Each path is a sequence of distinct nodes from source to
target.

## Usage

``` r
k_shortest_paths(x, from, to, k = 3, weights = NULL, directed = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- from:

  Character or numeric node identifier for the source node.

- to:

  Character or numeric node identifier for the target node.

- k:

  Integer; number of shortest paths to find. Default 3.

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

A list with class "cograph_k_paths" containing:

- paths:

  List of up to `k` character vectors, each containing node names in
  path order

- distances:

  Numeric vector of path lengths (sum of edge weights or hop count)

- from:

  Source node name

- to:

  Target node name

- k:

  Number of paths requested

## Details

Yen's algorithm finds the k shortest loopless (simple) paths in a graph.
It works by:

1.  Finding the shortest path via Dijkstra's algorithm

2.  For each subsequent path, systematically exploring deviations from
    previously found paths by temporarily removing edges, finding spur
    paths, and selecting the shortest candidate

The algorithm may return fewer than `k` paths if fewer distinct loopless
paths exist between the two nodes.

## References

Yen, J.Y. (1971). Finding the K shortest loopless paths in a network.
*Management Science*, 17(11), 712-716.
[doi:10.1287/mnsc.17.11.712](https://doi.org/10.1287/mnsc.17.11.712)

## See also

[`shortest_paths`](https://sonsoles.me/cograph/reference/shortest_paths.md)

## Examples

``` r
# Find 3 shortest paths in a small network
adj <- matrix(c(
  0, 1, 1, 0, 0,
  0, 0, 1, 1, 0,
  0, 0, 0, 1, 1,
  0, 0, 0, 0, 1,
  0, 0, 0, 0, 0
), 5, 5, byrow = TRUE)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
kp <- cograph::k_shortest_paths(adj, from = "A", to = "E", k = 3)
kp
#> K Shortest Paths
#> ================
#>   From: A 
#>   To: E 
#>   Requested: 3 paths
#>   Found: 3 paths
#> 
#>   Path 1 (distance = 2): A -> C -> E
#>   Path 2 (distance = 3): A -> B -> D -> E
#>   Path 3 (distance = 3): A -> C -> D -> E
```
