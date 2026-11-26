# Weight Neighborhood Centrality

Weight neighborhood centrality (Wang et al. 2017) adds to a benchmark
centrality of a node those of its neighbors, each weighted by the
importance of the edge relative to the average edge: \$\$C_i = \phi_i +
\sum\_{j \in N(i)} \frac{w\_{ij}}{\langle w \rangle} \phi_j, \qquad
w\_{ij} = (k_i k_j)^{\alpha},\$\$ with \\k\\ the degree, \\\langle w
\rangle\\ the mean of \\w\\ over the edges and \\\phi\\ the degree or
the coreness.

## Usage

``` r
centrality_weight_neighborhood(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `wnc_benchmark` (`\"degree\"` or `\"coreness\"`) and
  `wnc_alpha` (default 1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The source defines the measure for any benchmark centrality and reports
the degree and the coreness side by side; `wnc_benchmark` chooses,
`"degree"` by default (owner decision 2026-11-26). `wnc_alpha = 1` is
the authors' value. \\\langle w \rangle\\ is the mean over the edges,
the reading the source's Table 1 fixes, and is taken over the whole
network, so adding a component changes every score. On a network without
edges the score is the benchmark, 0. The implementation reproduces the
20 values \\C(k)\\ and \\C(k_s)\\ of the source's Table 1 for ten nodes
of its Fig. 2. The top of that figure is cut off in the copy read: five
of the 27 edges were inferred from the other columns of Table 1 (the
source audit gives the reasoning), and the nodes they add are not among
the ten. It also reproduces the monotonicity of both variants on the
dolphins network (0.9905, Table 3), which is below the largest value
possible there. On four test networks, with the degree as benchmark the
score ranks nodes closest to `diffusion` and the two-step expected
influence (mean Kendall correlation 0.95) and
[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md)
(0.94). With the coreness it ranks them closest to
[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md)
(0.97),
[`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md)
and `wksdn` (0.96), with correlations of 0.99 or more on single
networks: 0.997 with `laplacian`, 0.996 with `wksdn` and 0.995 with
`lnc` on one, and 1 with `ksgc` on a network with a single k-shell,
where the coreness is the same at every node. A `wnc_benchmark` other
than `"degree"` and `"coreness"`, or a `wnc_alpha` that is not a single
finite number, raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Wang, J., Hou, X., Li, K. and Ding, Y. (2017). A novel weight
neighborhood centrality algorithm for identifying influential spreaders
in complex networks. Physica A: Statistical Mechanics and its
Applications, 475, 88-105.
[doi:10.1016/j.physa.2017.02.007](https://doi.org/10.1016/j.physa.2017.02.007)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_wd`](https://sonsoles.me/cograph/reference/centrality_wd.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_weight_neighborhood(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   29.26039   40.85575   52.97922   38.67726   28.10513   31.07579   23.27628 
#>   Evaluate     Create      Share 
#>   35.03667   42.63814   35.03667 
centrality_weight_neighborhood(regulation_net, wnc_benchmark = "coreness")
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   21.82396   29.34963   38.19560   28.55746   21.16381   22.48411   16.67482 
#>   Evaluate     Create      Share 
#>   23.80440   30.14181   23.80440 
```
