# SALSA Authority Centrality

Stochastic Approach for Link-Structure Analysis. Returns authority
scores. Requires a directed graph.

## Usage

``` r
centrality_salsa(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).
  Must be directed.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of SALSA authority scores.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_authority`](https://sonsoles.me/cograph/reference/centrality_authority.md)
for HITS authority.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_salsa(adj)
#>   A   B   C 
#> 0.5 0.0 1.0 
```
