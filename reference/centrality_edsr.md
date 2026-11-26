# Extended Diversity-Strength Ranking

Extended diversity-strength ranking (EDSR; Zareie et al. 2019) sums the
diversity-strength ranking of the neighbors of a node: \$\$EDSR(v) =
\sum\_{u \in N(v)} DSR(u),\$\$ with \\DSR\\ the score
[`centrality_dsr`](https://sonsoles.me/cograph/reference/centrality_dsr.md)
returns. It reaches the nodes two steps away.

## Usage

``` r
centrality_edsr(x, ...)
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
The sum for a neighbor is not restricted, so it includes the node itself
once for each of its neighbors, as the equation of the source is
printed. Isolated nodes score 0. The implementation reproduces the
monotonicity of the ranking the source prints for the karate club
network (0.9542). `normalized = TRUE` divides the scores by their
maximum.

## References

Zareie, A., Sheikhahmadi, A. and Jalili, M. (2019). Influential node
ranking in social networks based on neighborhood diversity. Future
Generation Computer Systems, 94, 120-129.
[doi:10.1016/j.future.2018.11.023](https://doi.org/10.1016/j.future.2018.11.023)
.

## See also

[`centrality_dsc`](https://sonsoles.me/cograph/reference/centrality_dsc.md),
[`centrality_dsr`](https://sonsoles.me/cograph/reference/centrality_dsr.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_edsr(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   38.87042   41.75477   53.47684   37.74826   30.23173   39.81711   32.46192 
#>   Evaluate     Create      Share 
#>   41.92508   41.21828   39.99719 
```
