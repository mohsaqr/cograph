# Extended gravity centrality

Ma et al.'s extended gravity score is the sum of the immediate
neighbors' raw gravity scores: \\G^+(i)=\sum\_{j\in N(i)}G(j)\\, where
\\G(j)=\sum\_{l:0\<d(j,l)\le r}k_s(j)k_s(l)/d(j,l)^2\\. Core numbers and
hop distances are calculated on the original simple undirected graph.
The radius applies around each neighbor j; it is not a radius around the
focal node i. A contribution can therefore reach r+1 hops from i, and
paths from a neighbor back to i also contribute.

## Usage

``` r
centrality_extended_gravity(x, gravity_radius = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- gravity_radius:

  Nonnegative hop-distance cutoff, default 3. NULL or infinity includes
  the entire reachable component. The optional `"auto"` setting is a
  cograph extension: round half the mean finite positive hop distance to
  the nearest integer (ties to even), with minimum one. It is not a
  parameter rule from Ma et al.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive final scores are divided by their
  maximum.

## Value

Named numeric vector in input node order.

## Details

Default radius three is the setting used in the original paper. NULL or
infinity includes every reachable partner, excluding the gravity source
itself. Radius zero and isolates score zero. The outer neighbor sum has
no distance penalty. All inner scores remain raw until the final
optional max normalization.

Uses the simple undirected unweighted skeleton, with either direction
creating an edge, parallel edges counted once and loops removed. This
projection is a cograph convention for other inputs. Edge weights,
`mode`, `gravity_mass` and path-weight inversion do not affect this
measure: its masses are always k-shell indices. Computation includes
all-pairs hop distances, so it can be expensive for large graphs.

## References

Ma, L. L., Ma, C., Zhang, H. F., & Wang, B. H. (2016). Identifying
influential spreaders in complex networks based on gravity formula.
Physica A, 451, 205-212, equations 6 and 7.
[doi:10.1016/j.physa.2015.12.162](https://doi.org/10.1016/j.physa.2015.12.162)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md).

## Examples

``` r
centrality_extended_gravity(igraph::make_ring(6), gravity_radius = 3)
#>        1        2        3        4        5        6 
#> 20.88889 20.88889 20.88889 20.88889 20.88889 20.88889 
```
