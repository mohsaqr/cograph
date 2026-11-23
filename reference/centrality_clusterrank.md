# ClusterRank

ClusterRank (Chen et al. 2013, Eq. 3-4) damps the degrees of a node's
neighbors by its local clustering coefficient \\c_v\\: \$\$CR(v) =
10^{-c_v} \sum\_{u \in \Gamma(v)} (k_u + 1).\$\$

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

Edge weights are ignored. On an undirected network \\\Gamma(v)\\ holds
the neighbors, \\k\\ is the degree and \\c_v\\ the local clustering
coefficient. On a directed network the paper's directed form applies
(Eq. 1): a link \\v \to u\\ makes \\u\\ a follower of \\v\\,
\\\Gamma(v)\\ holds the followers (out-neighbors), \\k\\ is the
out-degree, and \\c_v\\ is the number of links among the followers over
\\k_v(k_v - 1)\\, a reciprocated pair counting as two; `mode = "in"`
reads the links the other way round. A node with fewer than two
neighbors has no clustering coefficient and returns `NaN`.
[`centiserve::clusterrank()`](https://rdrr.io/pkg/centiserve/man/clusterrank.html)
(and cograph 2.7.19 and earlier, which on a directed network also took
the undirected skeleton) multiply by \\c_v\\ itself instead of
\\10^{-c_v}\\.

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
#>   7.000000   8.933672   9.000000   7.494213   6.000000   3.478505   5.569907 
#>   Evaluate     Create      Share 
#>   4.641589   7.872779   6.034065 
```
