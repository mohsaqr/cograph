# Global and Local Information Centrality

The global and local information (GLI) method (Yang et al. 2020) gives
every node the mass \\Iks + DC\\, its improved k-shell index plus its
degree, and scores a node by its own share of the total mass and the
mass around it: \$\$I_i = \exp\Bigl\[\frac{Iks_i + DC_i}{\sum_l (Iks_l +
DC_l)}\Bigr\] \sum\_{j:\\1 \le d\_{ij} \le r} \frac{Iks_j +
DC_j}{d\_{ij}}.\$\$ The improved k-shell index is \\Iks = ks + nit\\,
the k-shell index plus the round of the k-shell decomposition in which
the node is removed.

## Usage

``` r
centrality_gli(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `gli_radius` (the distance range \\r\\, default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Rounds are counted from one through the whole decomposition, and all
nodes at or below the current degree leave in the same round, as in
Table 1 of the source. `gli_radius` is 3 by default, the value of the
source. The total mass is taken over all nodes, so adding a component
changes every score. The source assumes no isolated node; here an
isolated node is left out of the decomposition and scores 0. A
`gli_radius` that is not a whole number of at least one raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Yang, Y.-Z., Hu, M. and Huang, T.-Y. (2020). Influential nodes
identification in complex networks based on global and local
information. Chinese Physics B, 29(8), 088903.
[doi:10.1088/1674-1056/ab969f](https://doi.org/10.1088/1674-1056/ab969f)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_gli(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   97.89294  104.48803  111.51593  103.37051   95.48225   98.44290   91.83054 
#>   Evaluate     Create      Share 
#>  100.09278  106.16431  100.89033 
```
