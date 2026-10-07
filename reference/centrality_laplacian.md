# Laplacian Centrality

Laplacian centrality (Qi et al. 2012) is the drop in the Laplacian
energy of a network when a node is removed. Without edge weights the
drop is \$\$L(v) = k_v^2 + k_v + 2 \sum\_{u \in N(v)} k_u.\$\$

## Usage

``` r
centrality_laplacian(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `normalized` (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored, so the score is the unweighted case of the
weighted measure of Qi et al. (2012). On a directed network \\k\\ is the
total degree, in plus out, and the neighbor sum runs over out-neighbors.
A self-loop adds 2 to the degree. On undirected networks the values
equal
[`centiserve::laplacian()`](https://rdrr.io/pkg/centiserve/man/laplacian.html).
`normalized = TRUE` divides the scores by their maximum.

## References

Qi, X., Fuller, E., Wu, Q., Wu, Y., & Zhang, C.-Q. (2012). Laplacian
centrality: A new centrality measure for weighted networks. Information
Sciences, 194, 240-253.
[doi:10.1016/j.ins.2011.12.027](https://doi.org/10.1016/j.ins.2011.12.027)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_laplacian(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         66        118         98         72         70         68         62 
#>   Evaluate     Create      Share 
#>         70        106         84 
```
