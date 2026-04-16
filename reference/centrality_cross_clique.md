# Cross-Clique Connectivity

Count of all cliques (not just maximal) containing each node. Measures
embeddedness in dense substructures.

## Usage

``` r
centrality_cross_clique(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named integer vector of cross-clique counts.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_cross_clique(adj)
#> A B C 
#> 4 4 4 
```
