# Edge Clustering Coefficient Centrality

Edge clustering coefficient centrality (NC; Wang et al. 2012) sums, over
the edges of a node, the share of the triangles each edge could be part
of that it is part of: \$\$NC(u) = \sum\_{v \in N(u)}
\frac{z\_{uv}}{\min(k_u - 1, k_v - 1)},\$\$ with \\z\_{uv}\\ the number
of triangles on the edge \\(u, v)\\ and \\k\\ the degree.

## Usage

``` r
centrality_edge_clustering(x, ...)
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
There is no \\+1\\ in the numerator: the source's worked value \\ECC(1,
4) = 2/3\\ on its Fig. 1 rules out the form of Radicchi et al. An edge
with an end of degree one is in no triangle and its denominator is 0;
the source does not say what such an edge contributes, and it
contributes 0 here. Isolated nodes score 0. \\NC(u)\\ is at least the
degree times the local clustering coefficient, with equality when no
neighbor has a smaller degree than \\u\\. The source prints one edge
value and no node score, so the node scores are checked against the
definition only. On four test networks it ranks nodes closest to `lac`
and `redundancy` (mean Kendall correlation 0.90) and `mnc` (0.86).
`normalized = TRUE` divides the scores by their maximum.

## References

Wang, J., Li, M., Wang, H. and Pan, Y. (2012). Identification of
essential proteins based on edge clustering coefficient. IEEE/ACM
Transactions on Computational Biology and Bioinformatics, 9(4),
1070-1080.
[doi:10.1109/TCBB.2011.147](https://doi.org/10.1109/TCBB.2011.147) .

## See also

[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality_mnc`](https://sonsoles.me/cograph/reference/centrality_mnc.md),
[`centrality_support`](https://sonsoles.me/cograph/reference/centrality_truss.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_edge_clustering(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.500000   3.183333   5.250000   2.433333   1.583333   2.000000   2.000000 
#>   Evaluate     Create      Share 
#>   2.500000   3.650000   3.000000 
```
