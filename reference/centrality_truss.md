# Truss, Mixed-Degree Decomposition, Bridging Coefficient, Godfather and Support

The truss number of a node (Malliaros et al. 2016) is the largest truss
number of an incident edge, where a k-truss requires every edge to lie
in at least \\k-2\\ triangles of the subgraph. Mixed-degree
decomposition (Zeng and Zhang 2013) peels nodes by residual degree plus
`mdd_lambda` times exhausted degree. The bridging coefficient (Hwang et
al. 2008) is \\(1/d_i) / \sum\_{j \in N(i)} 1/d_j\\. The Godfather index
(Jackson 2020) counts the unordered pairs of neighbors with no edge
between them, and the support (Jackson 2020) counts the neighbors that
share at least one common neighbor with the node.

## Usage

``` r
centrality_truss(x, ...)

centrality_mdd(x, mdd_lambda = 0.7, ...)

centrality_bridging_coefficient(x, ...)

centrality_godfather(x, ...)

centrality_support(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

- mdd_lambda:

  Weight of the exhausted degree, between 0 and 1. Default 0.7, the
  value of the worked example of Zeng and Zhang (2013). A value outside
  that range raises an error.

## Value

A named numeric vector with one score per node, in input node order.

## Details

All five measures use the simple undirected skeleton, so direction,
weights, loops and parallel edges are ignored. Isolated nodes score 0.
An edge outside every triangle has truss number 2, and a node of a
complete graph on \\k\\ nodes has truss number \\k\\, as in NetworkX.
Sources that label trusses by the triangle threshold report values two
smaller. `mdd_lambda = 0` gives the k-core number and `mdd_lambda = 1`
gives the degree. The bridging coefficient is the factor that bridging
centrality multiplies with betweenness. LocalRank (Chen et al. 2012),
listed in the Centrality Zoo, is
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md).

## References

Malliaros, F. D., Rossi, M. E. G., & Vazirgiannis, M. (2016). Locating
influential nodes in complex networks. Scientific Reports, 6, 19307.
[doi:10.1038/srep19307](https://doi.org/10.1038/srep19307) .

Zeng, A., & Zhang, C. J. (2013). Ranking spreaders by decomposing
complex networks. Physics Letters A, 377, 1031-1035.
[doi:10.1016/j.physleta.2013.02.039](https://doi.org/10.1016/j.physleta.2013.02.039)
.

Hwang, W., Kim, T., Ramanathan, M., & Zhang, A. (2008). Bridging
centrality: graph mining from element level to group level. KDD '08,
336-344.
[doi:10.1145/1401890.1401934](https://doi.org/10.1145/1401890.1401934) .

Jackson, M. O. (2020). A typology of social capital and associated
network measures. Social Choice and Welfare, 54, 311-336.
[doi:10.1007/s00355-019-01189-3](https://doi.org/10.1007/s00355-019-01189-3)
.

Chen, D., Lu, L., Shang, M. S., Zhang, Y. C., & Zhou, T. (2012).
Identifying influential nodes in complex networks. Physica A, 391,
1777-1787.
[doi:10.1016/j.physa.2011.09.017](https://doi.org/10.1016/j.physa.2011.09.017)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_truss(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          3          4          4          3          3          3          3 
#>   Evaluate     Create      Share 
#>          4          4          4 
centrality_mdd(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        4.7        4.8        4.9        4.7        4.7        4.7        4.0 
#>   Evaluate     Create      Share 
#>        4.7        4.8        4.7 
centrality_bridging_coefficient(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2142857  0.1437372  0.1058201  0.1397206  0.2014388  0.2222222  0.3697183 
#>   Evaluate     Create      Share 
#>  0.2372881  0.1502146  0.2372881 
centrality_godfather(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          8         10         10          7          6          3 
#>   Evaluate     Create      Share 
#>          5          7          4 
centrality_support(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          6          7          6          5          5          4 
#>   Evaluate     Create      Share 
#>          5          6          5 
```
