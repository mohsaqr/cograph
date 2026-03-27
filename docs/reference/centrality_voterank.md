# VoteRank Centrality

Identifies influential spreaders via an iterative voting mechanism.
Returns normalized rank (1 = most influential). Based on Zhang et al.
(2016).

## Usage

``` r
centrality_voterank(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](http://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of VoteRank values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_voterank(adj)
#>         A         B         C 
#> 1.0000000 0.6666667 0.3333333 
```
