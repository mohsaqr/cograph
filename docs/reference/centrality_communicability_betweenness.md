# Communicability Betweenness Centrality

Fraction of total communicability that passes through each node.

## Usage

``` r
centrality_communicability_betweenness(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of communicability betweenness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_communicability_betweenness(adj)
#>         A         B         C 
#> 0.4978614 0.4978614 0.4978614 
```
