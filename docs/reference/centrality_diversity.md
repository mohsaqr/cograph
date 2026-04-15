# Diversity Centrality

Shannon entropy of the edge weight distribution per node. Measures how
evenly a node distributes its connections.

## Usage

``` r
centrality_diversity(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of diversity centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
mat <- matrix(c(0, .5, .3, .5, 0, .8, .3, .8, 0), 3, 3)
rownames(mat) <- colnames(mat) <- c("A", "B", "C")
centrality_diversity(mat)
#>         A         B         C 
#> 0.9544340 0.9612366 0.8453509 
```
