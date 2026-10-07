# X-Degree Centrality

X-degree (Torres et al. 2021, equation 3.15) counts the oriented
nonbacktracking walks of four edges whose middle node is \\i\\. It
depends only on the degrees \\d_j\\ of the neighbors of \\i\\:
\$\$Xdeg(i) = \Big(\sum\_{j \in N(i)} (d_j - 1)\Big)^2 - \sum\_{j \in
N(i)} (d_j - 1)^2.\$\$

## Usage

``` r
centrality_x_degree(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Isolated nodes and leaves score zero, and every node of a star scores
zero. Components are scored independently. The function computes the
score on the supplied network. The iterative node-removal immunization
procedure of the paper is a separate algorithm.

## References

Torres, L., Chan, K. S., Tong, H., & Eliassi-Rad, T. (2021).
Nonbacktracking Eigenvalues under Node Removal: X-Centrality and
Targeted Immunization. SIAM Journal on Mathematics of Data Science,
3(2), 656-675.
[doi:10.1137/20M1352132](https://doi.org/10.1137/20M1352132) .

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_dynamical_importance`](https://sonsoles.me/cograph/reference/centrality_dynamical_importance.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_x_degree(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        386        558        768        516        348        422        298 
#>   Evaluate     Create      Share 
#>        498        604        498 
```
