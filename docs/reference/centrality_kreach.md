# Geodesic K-Path Centrality

Count of nodes reachable within shortest path distance `k`. Measures how
many nodes a given node can reach quickly.

## Usage

``` r
centrality_kreach(x, mode = "all", k = 3, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- k:

  Maximum path length. Default 3.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`, `invert_weights`).

## Value

Named numeric vector of k-reach centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
centrality_kreach(adj, k = 2)
#> A B C D 
#> 2 3 3 2 
```
