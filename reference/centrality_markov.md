# Markov Centrality

Markov centrality (White and Smyth 2003) is the inverse of the mean
first passage time of a random walk into a node: \$\$M(j) =
\left(\frac{1}{n} \sum\_{i} m\_{ij}\right)^{-1},\$\$ where \\m\_{ij}\\
is the expected number of steps from \\i\\ to the first visit of \\j\\
and \\m\_{jj} = 0\\.

## Usage

``` r
centrality_markov(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The walk moves from a node to each of its out-neighbors with equal
probability, so edge weights are ignored. A disconnected network gives
`NA` for every node with a warning that carries no condition class. On a
directed network that is connected but not strongly connected some
scores are `NA` without a warning. The values equal
[`centiserve::markovcent()`](https://rdrr.io/pkg/centiserve/man/markovcent.html).

## References

White, S., & Smyth, P. (2003). Algorithms for estimating relative
importance in networks. In Proceedings of the Ninth ACM SIGKDD
International Conference on Knowledge Discovery and Data Mining (pp.
266-275).
[doi:10.1145/956750.956782](https://doi.org/10.1145/956750.956782) .

## See also

[`centrality_random_walk`](https://sonsoles.me/cograph/reference/centrality_random_walk.md),
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_markov(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.19359291 0.06433656 0.24757007 0.20391000 0.15165989 0.07197797 0.05356390 
#>   Evaluate     Create      Share 
#> 0.04778456 0.14198438 0.15182543 
```
