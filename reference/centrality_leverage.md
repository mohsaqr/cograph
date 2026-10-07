# Leverage Centrality

Leverage centrality (Joyce et al. 2010) compares the degree of a node
with the degrees of its neighbors: \$\$l_i = \frac{1}{\|N(i)\|} \sum\_{j
\in N(i)} \frac{k_i - k_j}{k_i + k_j}.\$\$ Positive values mark nodes
with more ties than their typical neighbor.

## Usage

``` r
centrality_leverage(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. `mode` selects both the degree and the
neighbor set, and with `mode = "all"` on a directed network the degree
is in plus out. The score lies between -1 and 1. An isolated node is
`NaN`. On undirected networks the values equal
[`centiserve::leverage()`](https://rdrr.io/pkg/centiserve/man/leverage.html).

## References

Joyce, K. E., Laurienti, P. J., Burdette, J. H., & Hayasaka, S. (2010).
A new measure of centrality for brain networks. PLoS ONE, 5(8), e12200.
[doi:10.1371/journal.pone.0012200](https://doi.org/10.1371/journal.pone.0012200)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_neighborhood_connectivity`](https://sonsoles.me/cograph/reference/centrality_neighborhood_connectivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_leverage(regulation_net)
#>      Explore         Plan      Monitor        Adapt      Reflect      Discuss 
#>  0.002797203  0.102719503  0.160858189  0.039826840  0.047792208 -0.121212121 
#>   Synthesize     Evaluate       Create        Share 
#> -0.251515152 -0.149184149  0.070085470 -0.059340659 
```
