# Laplacian Centrality

Laplacian centrality (Qi et al. 2012) is the drop in the Laplacian
energy of a network when a node is removed. On a weighted undirected
network, with strengths \\x\\, the drop is \$\$L(v) = x_v^2 + \sum\_{u}
w\_{uv}^2 + 2 \sum\_{u} x_u w\_{uv};\$\$ without edge weights it is
\$\$L(v) = k_v^2 + k_v + 2 \sum\_{u \in N(v)} k_u.\$\$

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

On a weighted undirected network the weights are read as in Qi et al.
(2012) and the values equal `networkx::laplacian_centrality(weight =)`
(cograph 2.7.19 and earlier ignored the weights); self-loops cancel in
the Laplacian. `weighted = FALSE` gives the unweighted formula, where a
self-loop adds 2 to the degree. On a directed network the weights are
ignored, \\k\\ is the total degree, in plus out, and the neighbor sum
runs over out-neighbors. On unweighted undirected networks the values
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
