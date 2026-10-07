# Clustering Degree Algorithm

The clustering degree algorithm (CDA; Wang et al. 2018) scores a node by
its clustering degree \\CD_i = \[\alpha d_i + (1-\alpha) s_i\] / \[1 +
\exp(-C_i^w)\]\\ plus the clustering degrees of its neighbors, each
scaled by the edge weight: \$\$PC_i = CD_i + \sum\_{j \in N(i)}
\frac{w\_{ij}}{w\_{\max}} CD_j.\$\$ Here \\d_i\\ is the degree, \\s_i\\
the strength, \\C_i^w\\ the weighted clustering coefficient of Barrat et
al., and \\w\_{\max}\\ the largest edge weight in the network.

## Usage

``` r
centrality_cda(x, cda_alpha = 0.5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- cda_alpha:

  Weight \\\alpha\\ of the degree against the strength, between 0 and 1.
  Default 0.5, the value of Wang et al. (2018). A value outside that
  range raises an error.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`) and `normalized` (default
  `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure works on an undirected weighted network. On a directed
network the weights of the two arcs between a pair are added, and
`weighted = FALSE` uses the simple undirected skeleton. Self-loops are
removed, and `mode` and `invert_weights` have no effect. A node with
fewer than two neighbors has clustering 0, and an isolated node scores
0. Negative or non-finite weights raise an error. With `cda_alpha = 1`
the scores are unchanged when every weight is multiplied by a constant,
and with `cda_alpha = 0` they scale by that constant. On an unweighted
network `cda_alpha` has no effect.

## References

Wang, Q., Ren, J., Wang, Y., Zhang, B., Cheng, Y., & Zhao, X. (2018).
CDA: A Clustering Degree Based Influential Spreader Identification
Algorithm in Weighted Complex Network. IEEE Access, 6, 19550-19559.
[doi:10.1109/ACCESS.2018.2822844](https://doi.org/10.1109/ACCESS.2018.2822844)
.

## See also

[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md),
[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_cda(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   7.022374   9.542769  10.046776   8.693597   6.431974   7.688643   4.234239 
#>   Evaluate     Create      Share 
#>   9.335607   9.197362  10.460267 
```
