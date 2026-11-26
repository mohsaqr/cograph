# Diversity-Strength Ranking

Diversity-strength ranking (DSR; Zareie et al. 2019) sums the
diversity-strength centrality of the neighbors of a node: \$\$DSR(v) =
\sum\_{u \in N(v)} DSC(u),\$\$ with \\DSC\\ the score
[`centrality_dsc`](https://sonsoles.me/cograph/reference/centrality_dsc.md)
returns.

## Usage

``` r
centrality_dsr(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Isolated nodes score 0. The implementation reproduces the monotonicity
of the ranking the source prints for the karate club network (0.9403).
`normalized = TRUE` divides the scores by their maximum.

## References

Zareie, A., Sheikhahmadi, A. and Jalili, M. (2019). Influential node
ranking in social networks based on neighborhood diversity. Future
Generation Computer Systems, 94, 120-129.
[doi:10.1016/j.future.2018.11.023](https://doi.org/10.1016/j.future.2018.11.023)
.

## See also

[`centrality_dsc`](https://sonsoles.me/cograph/reference/centrality_dsc.md),
[`centrality_edsr`](https://sonsoles.me/cograph/reference/centrality_edsr.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dsr(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   5.456657   8.617136   7.564633   8.895604   7.384545   5.610579   5.147117 
#>   Evaluate     Create      Share 
#>   6.452745   9.463163   7.516528 
```
