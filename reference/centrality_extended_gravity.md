# Extended Gravity Centrality

Extended gravity centrality (Ma et al. 2016) is the sum of the gravity
scores of a node's neighbors, \$\$G^+(i) = \sum\_{j \in N(i)} G(j),
\qquad G(j) = \sum\_{l:\\ 0 \< d\_{jl} \le r} \frac{k_s(j)\\
k_s(l)}{d\_{jl}^2},\$\$ where \\k_s\\ is the k-shell index and \\d\\ the
hop distance.

## Usage

``` r
centrality_extended_gravity(x, gravity_radius = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- gravity_radius:

  Hop radius \\r\\: a nonnegative number (default 3, the value of Ma et
  al. 2016), `"auto"`, or `NULL` or `Inf` for the whole component.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored, and the masses are always k-shell
indices. The radius applies around each neighbor \\j\\, so a
contribution can come from \\r+1\\ hops away from \\i\\, including paths
back to \\i\\. Radius 0 and isolated nodes give 0. The `"auto"` radius
is half the mean finite positive hop distance, rounded to the nearest
integer with a minimum of 1. This rule is a package choice. A negative
radius raises an error.

## References

Ma, L. L., Ma, C., Zhang, H. F., & Wang, B. H. (2016). Identifying
influential spreaders in complex networks based on gravity formula.
Physica A, 451, 205-212.
[doi:10.1016/j.physa.2015.12.162](https://doi.org/10.1016/j.physa.2015.12.162)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_extended_coreness`](https://sonsoles.me/cograph/reference/centrality_extended_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_extended_gravity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        504        600        696        588        492        516        432 
#>   Evaluate     Create      Share 
#>        540        612        540 
```
