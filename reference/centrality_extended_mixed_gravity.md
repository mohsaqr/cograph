# Extended mixed gravitational centrality

Extended mixed gravitational centrality (EMGC), also called IGC+, sums
the raw MGC scores of immediate neighbors: \\EMGC_i=\sum\_{j\in
N(i)}MGC_j\\. Each inner MGC score uses its own source node j's core
number, partner degrees, and original-graph hop distances. The inner
radius is centered on j, so a contribution can reach r+1 hops from i.
Paths from j back to i are included. The outer neighbor sum has no
distance or mass factor.

## Usage

``` r
centrality_extended_mixed_gravity(x, gravity_radius = 3, ...)
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

Follows the reproduction in Li and Huang (2022), equation 8, attributed
to Wang et al. (2018); the original full equations and software have not
been inspected. Uses the same skeleton and radius conventions as
[`centrality_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_mixed_gravity.md).
Default inner radius three follows the reproduced definition; radius one
matches the Zoo's literal inner neighbor sum. Optional maximum
normalization occurs only after summing raw neighbor scores. Isolates
and radii below one score zero. Empty and singleton graphs give no
scores and zero, respectively. Dense O(n cubed) time and O(n squared)
memory. Verification of these numerical equations does not establish
author-software parity or predictive superiority.

## References

Wang, J., Li, C. and Xia, C. (2018).
[doi:10.1016/j.amc.2018.04.028](https://doi.org/10.1016/j.amc.2018.04.028)
. Definition read in Li, Z. and Huang, X. (2022), Scientific Reports,
12, 9879, equations 5-8 and reference 19.
[doi:10.1038/s41598-022-14005-3](https://doi.org/10.1038/s41598-022-14005-3)
.

## Examples

``` r
centrality_extended_mixed_gravity(igraph::make_ring(6))
#>        1        2        3        4        5        6 
#> 20.88889 20.88889 20.88889 20.88889 20.88889 20.88889 
```
