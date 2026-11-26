# Icentr

Icentr (Mussone et al. 2022) scores a node by all the edges of its
component, each counted less the farther it lies from the node. Every
node gets a level, its distance in steps from the focal node. An edge
between two levels takes the level of its farther end, and an edge
inside one level takes that level plus one. The score is \$\$Icentr(i) =
\sum\_{e} 2^{-(lev(e) - 1)}\$\$ over the edges of the component of
\\i\\.

## Usage

``` r
centrality_icentr(x, ...)
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

This is the unweighted form of the source, with unit node and edge
weights. It is computed on the simple undirected skeleton, so direction,
weights, loops and parallel edges are ignored. Each edge at the node
itself adds 1. Isolated nodes score 0. `normalized = TRUE` divides the
scores by their maximum; the source rescales to the range from 0 to 1 by
the minimum and maximum instead.

## References

Mussone, L., Viseh, H. and Notari, R. (2022). Novel centrality measures
and applications to underground networks. Physica A: Statistical
Mechanics and its Applications, 589, 126595.
[doi:10.1016/j.physa.2021.126595](https://doi.org/10.1016/j.physa.2021.126595)
.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_icentr(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>      14.75      16.00      16.75      16.25      15.00      15.25      14.00 
#>   Evaluate     Create      Share 
#>      15.50      16.00      15.25 
```
