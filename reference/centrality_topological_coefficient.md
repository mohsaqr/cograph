# Topological Coefficient

The topological coefficient (Stelzl et al. 2005) measures how far a node
shares neighbors with the nodes it is linked to through a common
neighbor: \$\$T(v) = \frac{\sum\_{u \in U_v} J(v, u)}{\|U_v\|\\
k_v},\$\$ where \\U_v\\ is the set of nodes that share at least one
neighbor with \\v\\, \\J(v, u)\\ is the number of shared neighbors plus
one when \\u\\ and \\v\\ are linked, and \\k_v\\ is the degree.

## Usage

``` r
centrality_topological_coefficient(x, ...)
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

Edge weights and direction are ignored. A node that shares no neighbor
with any other node scores 0, as does an isolated node. On undirected
networks the values equal
[`centiserve::topocoefficient()`](https://rdrr.io/pkg/centiserve/man/topocoefficient.html).

## References

Stelzl, U., Worm, U., Lalowski, M., Haenig, C., Brembeck, F. H.,
Goehler, H., et al. (2005). A human protein-protein interaction network:
A resource for annotating the proteome. Cell, 122(6), 957-968.
[doi:10.1016/j.cell.2005.08.029](https://doi.org/10.1016/j.cell.2005.08.029)
.

## See also

[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality_lac`](https://sonsoles.me/cograph/reference/centrality_lac.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_topological_coefficient(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6111111  0.6031746  0.6250000  0.6296296  0.5740741  0.7111111  0.7500000 
#>   Evaluate     Create      Share 
#>  0.7555556  0.6666667  0.7037037 
```
