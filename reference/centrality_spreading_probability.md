# Spreading Probability Centrality

Spreading probability centrality (SP; Bao et al. 2017) sums, over the
nodes within a radius of a node, the number of shortest paths to each,
discounted by the mean degree of the network once per step: \$\$SP(i) =
\sum\_{j:\\1 \le d\_{ij} \le r} \sigma\_{ij} \langle k
\rangle^{-d\_{ij}},\$\$ with \\\sigma\_{ij}\\ the number of shortest
paths between \\i\\ and \\j\\, \\d\_{ij}\\ their distance and \\\langle
k \rangle\\ the mean degree. The reciprocal of the mean degree stands
for the probability that an infection crosses one tie.

## Usage

``` r
centrality_spreading_probability(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `sp_radius` (the largest distance summed over,
  default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`sp_radius` is 3 by default, the value the source uses; `Inf` sums over
every node that can be reached. With `sp_radius = 1` the score is the
degree divided by the mean degree. The node itself is not counted.
Isolated nodes score 0, and so does every node of a network without
ties. The source prints no value of the measure on a small network, so
the implementation is checked against the definition only. A `sp_radius`
that is neither a whole number of at least one nor `Inf` raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Bao, Z.-K., Ma, C., Xiang, B.-B. and Zhang, H.-F. (2017). Identification
of influential nodes in complex networks: Method from spreading
probability viewpoint. Physica A: Statistical Mechanics and its
Applications, 468, 391-397.
[doi:10.1016/j.physa.2016.10.086](https://doi.org/10.1016/j.physa.2016.10.086)
.

## See also

[`centrality_kpath`](https://sonsoles.me/cograph/reference/centrality_local_efficiency.md),
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_spreading_probability(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.337449   1.522634   1.570645   1.625514   1.440329   1.440329   1.220850 
#>   Evaluate     Create      Share 
#>   1.440329   1.488340   1.371742 
centrality_spreading_probability(regulation_net, sp_radius = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.337449   1.522634   1.570645   1.625514   1.440329   1.440329   1.220850 
#>   Evaluate     Create      Share 
#>   1.440329   1.488340   1.371742 
```
