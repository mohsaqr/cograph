# Degree and Importance of Lines

The degree and importance of lines (DIL; Liu et al. 2016) adds to the
degree of a node a share of the importance of every line incident to it.
A line \\e\_{ij}\\ that lies on \\p\\ triangles has importance \\I\_{ij}
= (k_i - p - 1)(k_j - p - 1) / \lambda\\ with \\\lambda = p/2 + 1\\, and
this importance is split between the endpoints in proportion to their
degrees minus one: \$\$L_i = k_i + \sum\_{j \in \Gamma_i} I\_{ij}
\frac{k_i - 1}{k_i + k_j - 2}.\$\$

## Usage

``` r
centrality_dil(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Every score is at least the degree of the node, and isolated nodes score
0. The share is undefined only on an isolated edge, whose importance is
zero. That share is set to zero, so both endpoints score 1. The text
layer of the published article reads \\\lambda\\ as \\2p + 1\\. The
printed page and the worked example of the article give \\p/2 + 1\\,
which is the value used here. `normalized = TRUE` divides the scores by
their maximum.

## References

Liu, J., Xiong, Q., Shi, W., Shi, X. and Wang, K. (2016). Evaluating the
importance of nodes in complex networks. Physica A: Statistical
Mechanics and its Applications, 452, 209-219.
[doi:10.1016/j.physa.2016.02.049](https://doi.org/10.1016/j.physa.2016.02.049)
.

## See also

[`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md),
[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dil(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  11.866667  17.580808  13.101818  22.646465  17.885714  16.644444   9.714286 
#>   Evaluate     Create      Share 
#>  14.222222  12.702020   9.502222 
```
