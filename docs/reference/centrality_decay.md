# Decay Centrality

Sum of delta^d over all nodes, where d is the shortest path distance.
Nodes near many others get higher scores. The `decay_parameter` controls
the distance penalty.

## Usage

``` r
centrality_decay(x, mode = "all", decay_parameter = 0.5, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- decay_parameter:

  Numeric between 0 and 1. Default 0.5.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of decay centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_decay(adj, decay_parameter = 0.5)
#>    A    B    C 
#> 1.75 2.00 1.75 
```
