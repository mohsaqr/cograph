# Residual Closeness Centrality

Sum of 1/2^d for all nodes, including self. Robust to disconnected
graphs.

## Usage

``` r
centrality_residual_closeness(x, mode = "all", ...)
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

Named numeric vector of residual closeness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_dangalchev`](https://sonsoles.me/cograph/reference/centrality_dangalchev.md)
(alias).

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_residual_closeness(adj)
#>    A    B    C 
#> 1.75 2.00 1.75 
```
