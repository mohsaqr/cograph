# Degree Centrality

Number of edges connected to each node. For directed networks,
`centrality_indegree` counts incoming edges and `centrality_outdegree`
counts outgoing edges.

## Usage

``` r
centrality_degree(x, mode = "all", ...)

centrality_indegree(x, ...)

centrality_outdegree(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`).

## Value

Named numeric vector of degree values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md)
for the weighted version.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_degree(adj)
#> A B C 
#> 2 2 2 
```
