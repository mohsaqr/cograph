# Multi-Characteristics Gravity Model Centrality

MCGM (Li and Huang 2022) is a gravity model whose node mass combines
degree \\K\\, core number \\S\\ and eigenvector centrality \\X\\, each
divided by its maximum over the network: \$\$MCGM_i = \sum\_{j : 0 \<
d(i,j) \le R} \frac{m_i m_j}{d(i,j)^2}, \qquad m_i = K_i + \alpha S_i +
X_i.\$\$ By default \\\alpha\\ is the larger of the medians of \\K\\ and
\\X\\ divided by the median of \\S\\ (equation 17).

## Usage

``` r
centrality_mcgm(x, mcgm_radius = 2, mcgm_alpha = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mcgm_radius:

  Hop-distance cutoff \\R\\. Default 2, the setting recommended in the
  paper. `NULL` or `Inf` includes every reachable node.

- mcgm_alpha:

  `NULL` (default) uses the median-based \\\alpha\\. A finite
  nonnegative number replaces it.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `normalized` (divide by the maximum, default
  `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored,
and `gravity_mass` and `gravity_radius` have no effect. On a
disconnected network the eigenvector feature is the projection of the
all-ones vector onto the dominant eigenspace, so a component with a
smaller spectral radius has \\X = 0\\. When the median core number is
zero the automatic \\\alpha\\ is undefined, and an error asks for
`mcgm_alpha`. `mcgm_alpha = 1` gives equation 16 of the paper. Isolated
nodes score zero, and an edgeless network or a radius below one gives
zero scores.

## References

Li, Z. and Huang, X. (2022). Identifying influential spreaders by
gravity model considering multi-characteristics of nodes. Scientific
Reports, 12, 9879.
[doi:10.1038/s41598-022-14005-3](https://doi.org/10.1038/s41598-022-14005-3)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_mixed_gravity`](https://sonsoles.me/cograph/reference/centrality_mixed_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mcgm(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   31.33187   39.82505   48.44388   38.76606   30.69390   31.88066   25.46942 
#>   Evaluate     Create      Share 
#>   33.55290   40.47895   33.62893 
```
