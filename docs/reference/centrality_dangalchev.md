# Dangalchev Closeness Centrality

Alias for residual closeness centrality: sum of 1/2^d.

## Usage

``` r
centrality_dangalchev(x, mode = "all", ...)
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

Named numeric vector of Dangalchev closeness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_residual_closeness`](https://sonsoles.me/cograph/reference/centrality_residual_closeness.md)
(equivalent).

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_dangalchev(adj)
#>    A    B    C 
#> 1.75 2.00 1.75 
```
