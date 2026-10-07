# ClusterRank

ClusterRank (Chen et al. 2013) combines the local clustering coefficient
\\c_v\\ of a node with the degrees of its neighbors: \$\$CR(v) = c_v
\sum\_{u \in N(v)} (k_u + 1).\$\$

## Usage

``` r
centrality_clusterrank(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. `mode` sets both the neighbor set and the
degrees, and on a directed network a reciprocated neighbor enters the
sum twice under `mode = "all"`. The clustering coefficient ignores edge
direction. A node with fewer than two neighbors has no clustering
coefficient and returns `NaN`. Chen et al. weight the sum by
\\10^{-c_v}\\, and the measure here multiplies by \\c_v\\ itself, as the
centiserve package does.

## References

Chen, D.-B., Gao, H., Lu, L., & Zhou, T. (2013). Identifying influential
nodes in large-scale directed networks: The role of clustering. PLoS
ONE, 8(10), e77455.
[doi:10.1371/journal.pone.0077455](https://doi.org/10.1371/journal.pone.0077455)
.

## See also

[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality_expected`](https://sonsoles.me/cograph/reference/centrality_expected.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_clusterrank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   21.00000   22.40000   29.33333   13.33333   12.00000   14.80000   15.50000 
#>   Evaluate     Create      Share 
#>   19.50000   27.73333   28.20000 
```
