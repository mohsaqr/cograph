# Extended Mixed Gravitational Centrality

Extended mixed gravitational centrality (Wang, Li and Xia 2018), also
called IGC+, sums the mixed gravitational scores of the immediate
neighbors of a node: \$\$EMGC_i = \sum\_{j \in N(i)} MGC_j.\$\$

## Usage

``` r
centrality_extended_mixed_gravity(x, gravity_radius = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- gravity_radius:

  Hop-distance cutoff of the inner scores. Default 3. `NULL` or `Inf`
  includes every reachable node.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Each inner score \\MGC_j\\ uses the radius around \\j\\, so a
contribution can come from up to `gravity_radius + 1` hops from the
focal node. The implementation follows the reproduction in Li and Huang
(2022), equation 8. The input handling and radius options of
[`centrality_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_mixed_gravity.md)
apply, so direction, weights, loops and parallel edges are ignored.
Isolated nodes score zero.

## References

Wang, J., Li, C. and Xia, C. (2018). Improved centrality indicators to
characterize the nodal spreading capability in complex networks. Applied
Mathematics and Computation, 334, 388-400.
[doi:10.1016/j.amc.2018.04.028](https://doi.org/10.1016/j.amc.2018.04.028)
.

Li, Z. and Huang, X. (2022). Identifying influential spreaders by
gravity model considering multi-characteristics of nodes. Scientific
Reports, 12, 9879.
[doi:10.1038/s41598-022-14005-3](https://doi.org/10.1038/s41598-022-14005-3)
.

## See also

[`centrality_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_mixed_gravity.md),
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_extended_mixed_gravity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        687        838        959        821        682        689        570 
#>   Evaluate     Create      Share 
#>        717        843        720 
```
