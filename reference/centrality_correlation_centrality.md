# Correlation Centrality

Correlation centrality (Fan et al. 2013) sums the efficiency of the
other nodes, each weighted by the number of shortest paths to it and
discounted by a power of the distance: \$\$CoC(i) = \frac{1}{n} \sum\_{j
\ne i} EC(j) \frac{\sigma\_{ij}}{d\_{ij}^{\alpha}}, \qquad EC(j) =
\frac{1}{n} \sum\_{k \ne j} \frac{1}{d\_{jk}},\$\$ with \\\sigma\_{ij}\\
the number of shortest paths between \\i\\ and \\j\\ and \\d\_{ij}\\
their distance.

## Usage

``` r
centrality_correlation_centrality(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `coc_alpha` (the exponent of the distance, default
  3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`coc_alpha` is 3 by default, the value the source selects. With that
value the direct neighbors carry most of the score. A node that cannot
be reached contributes 0, and isolated nodes score 0. The implementation
reproduces the 21 values the source prints for the ARPA network, on an
edge list recovered from the other columns of the same table. A
`coc_alpha` that is not a single finite nonnegative number raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Fan, W., Liu, Z. and Hu, P. (2013). Identifying node importance based on
information entropy in complex networks. Physica Scripta, 88(6), 065201.
[doi:10.1088/0031-8949/88/06/065201](https://doi.org/10.1088/0031-8949/88/06/065201)
.

## See also

[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality_niccm`](https://sonsoles.me/cograph/reference/centrality_niccm.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_correlation_centrality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   0.470625   0.538125   0.570000   0.562500   0.494375   0.499375   0.423750 
#>   Evaluate     Create      Share 
#>   0.503750   0.533125   0.486875 
```
