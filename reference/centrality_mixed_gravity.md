# Mixed Gravitational Centrality

Mixed gravitational centrality (Wang, Li and Xia 2018), also called
improved gravitational centrality, uses the core number \\k_s(i)\\ of
the focal node and the degree \\k(j)\\ of each partner node as masses:
\$\$MGC_i = k_s(i) \sum\_{j : 0 \< d(i,j) \le r}
\frac{k(j)}{d(i,j)^2}.\$\$

## Usage

``` r
centrality_mixed_gravity(x, gravity_radius = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- gravity_radius:

  Hop-distance cutoff \\r\\. Default 3. `NULL` or `Inf` includes every
  reachable node, and a value below one gives zero scores.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored,
and `gravity_mass` has no effect. The implementation follows the
reproduction of the method in Li and Huang (2022), equations 5-8. The
Centrality Zoo writes the inner sum over immediate neighbors, which
corresponds to `gravity_radius = 1`. Isolated nodes score zero, and
unreachable nodes contribute nothing. `gravity_radius = "auto"` is a
cograph heuristic that rounds half the mean finite distance to the
nearest integer, with a minimum of one. A negative radius raises an
error.

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

[`centrality_extended_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_extended_mixed_gravity.md),
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mixed_gravity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        130        144        158        141        127        133        122 
#>   Evaluate     Create      Share 
#>        139        147        139 
```
