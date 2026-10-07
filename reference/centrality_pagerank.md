# PageRank Centrality

PageRank (Brin and Page 1998) is the stationary distribution of a random
walk that follows an out-edge with probability \\d\\, choosing edges in
proportion to their weights, and otherwise jumps to a node drawn from
the reset distribution \\p\\: \$\$PR = (1 - d)\\p + d\\P^{T} PR,\$\$
where \\P\\ is the row-normalized weight matrix and \\d\\ is `damping`.

## Usage

``` r
centrality_pagerank(x, damping = 0.85, personalized = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- damping:

  Probability \\d\\ of following an edge (default 0.85).

- personalized:

  Reset distribution \\p\\, a non-negative numeric vector with one entry
  per node in input node order. The default `NULL` gives the uniform
  distribution.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are always used, and `weighted = FALSE` has no effect. A
node without out-edges passes its score to the reset distribution. The
scores sum to one and equal
[`igraph::page_rank()`](https://r.igraph.org/reference/page_rank.html).
The vector `personalized` is rescaled to sum to one and matched to nodes
by position; its names are ignored. A negative weight raises a
`cograph_negative_weights` error, an invalid `personalized` a
`cograph_bad_input` error, and a `damping` outside \\\[0, 1\]\\ an
error.

## References

Brin, S., & Page, L. (1998). The anatomy of a large-scale hypertextual
Web search engine. Computer Networks and ISDN Systems, 30(1-7), 107-117.
[doi:10.1016/S0169-7552(98)00110-X](https://doi.org/10.1016/S0169-7552%2898%2900110-X)
.

## See also

[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_leaderrank`](https://sonsoles.me/cograph/reference/centrality_leaderrank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_pagerank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.11846147 0.03640953 0.18376724 0.12356096 0.12513119 0.06803638 0.03760071 
#>   Evaluate     Create      Share 
#> 0.07386400 0.13821279 0.09495573 
```
