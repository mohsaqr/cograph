# Density Centrality

Density centrality (Ibnoulouafi and El Haziti 2018) treats the degree of
a node as a mass spread over discs whose radii are the distances to the
nodes around it: \$\$DNC(i) = \sum\_{j:\\1 \le d\_{ij} \le r}
\frac{k_i}{\pi d\_{ij}^2},\$\$ with \\k_i\\ the degree of the node and
\\d\_{ij}\\ the shortest-path distance.

## Usage

``` r
centrality_density_centrality(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `density_radius` (the largest distance summed over,
  default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`density_radius` is 3 by default, the setting the Centrality Zoo reports
for the source; `Inf` sums over every node that can be reached. The
formula was taken from Ullah et al. (2021), who reproduce it; the 2018
article was not consulted, so the default radius is not confirmed
against it. Isolated nodes score 0. A `density_radius` that is neither a
whole number of at least one nor `Inf` raises a `cograph_bad_parameter`
error. `normalized = TRUE` divides the scores by their maximum.

## References

Ibnoulouafi, A. and El Haziti, M. (2018). Density centrality:
identifying influential nodes based on area density formula. Chaos,
Solitons and Fractals, 114, 69-80.
[doi:10.1016/j.chaos.2018.06.022](https://doi.org/10.1016/j.chaos.2018.06.022)
.

Ullah, A., Wang, B., Sheng, J., Long, J., Khan, N. and Sun, Z. (2021).
Identification of nodes influence based on global structure model in
complex networks. Scientific Reports, 11, 6173.
[doi:10.1038/s41598-021-84684-x](https://doi.org/10.1038/s41598-021-84684-x)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_density_centrality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   9.549297  12.891550  16.711269  12.891550   9.549297   9.549297   6.684508 
#>   Evaluate     Create      Share 
#>   9.549297  12.891550   9.549297 
```
