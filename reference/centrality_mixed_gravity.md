# Mixed gravitational centrality

Mixed gravitational centrality (MGC), also called improved gravitational
centrality (IGC), uses the focal node's core number as its mass and the
partner node's degree as its mass: \\MGC_i=k_s(i)\sum\_{j:0\<d(i,j)\le
r}k(j)/d(i,j)^2\\. All degrees, core numbers and hop distances are
measured on the original simple undirected graph. The masses are
asymmetric even though distances are symmetric. This differs from using
core numbers on both ends or degree on both ends of each interaction.

## Usage

``` r
centrality_mixed_gravity(x, gravity_radius = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- gravity_radius:

  Nonnegative hop-distance cutoff, default three. NULL or infinity
  includes every reachable partner. Fractional cutoffs include exactly
  integer hop distances not exceeding them; values below one give zero.
  The optional `"auto"` is a cograph heuristic: round half the mean
  finite positive distance to the nearest integer (ties to even), with
  minimum one. It is not the cited radius rule and can change when
  disconnected components are added.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

The implementation follows the explicit reproduction of Wang et al.'s
method in Li and Huang (2022), equations 5-8, with default radius three.
The original 2018 full equations and author software have not been
inspected. The Zoo summary writes an immediate-neighbor inner sum;
`gravity_radius = 1` reproduces that literal interpretation. Numerical
verification establishes agreement with the cited reproduced definition,
not parity with unavailable original software or a guarantee of
spreading performance.

Uses the simple undirected unweighted skeleton: either arc creates an
edge, parallel edges count once and loops are removed. This projection
is a cograph convention outside the source domain. Edge weights, mode,
cutoff, gravity_mass and path-weight inversion are ignored. Isolates and
singleton graphs score zero; empty graphs return no scores. Unreachable
partners contribute zero. With a fixed radius, adding a disconnected
component leaves existing raw scores unchanged. Optional maximum
normalization applies to the complete result over all nodes. Dense
all-pairs distances cost O(n cubed) time and O(n squared) memory.

## References

Wang, J., Li, C. and Xia, C. (2018). Improved centrality indicators to
characterize the nodal spreading capability in complex networks. Applied
Mathematics and Computation, 334, 388-400.
[doi:10.1016/j.amc.2018.04.028](https://doi.org/10.1016/j.amc.2018.04.028)
.

Definition read in Li, Z. and Huang, X. (2022). Identifying influential
spreaders by gravity model considering multi-characteristics of nodes.
Scientific Reports, 12, 9879. Equations 5-8 and reference 19.
[doi:10.1038/s41598-022-14005-3](https://doi.org/10.1038/s41598-022-14005-3)
.

## See also

[`centrality_extended_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_extended_mixed_gravity.md).

## Examples

``` r
centrality_mixed_gravity(igraph::make_ring(6))
#>        1        2        3        4        5        6 
#> 10.44444 10.44444 10.44444 10.44444 10.44444 10.44444 
centrality_mixed_gravity(igraph::make_star(6), gravity_radius = 1)
#> 1 2 3 4 5 6 
#> 5 5 5 5 5 5 
```
