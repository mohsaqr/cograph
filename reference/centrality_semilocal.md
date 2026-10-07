# Semi-Local Centrality

Semi-local centrality (Chen et al. 2012) sums, over the neighbors \\u\\
of a node and the neighbors \\w\\ of each \\u\\, the number \\N(w)\\ of
nodes within two steps of \\w\\: \$\$C_L(v) = \sum\_{u \in \Gamma(v)}
\sum\_{w \in \Gamma(u)} N(w).\$\$

## Usage

``` r
centrality_semilocal(x, mode = "all", ...)
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

Edge weights are ignored. `mode` selects the neighbors, and with
`mode = "all"` on a directed network a reciprocated tie counts twice. An
isolated node scores 0. On undirected networks the values equal
[`centiserve::semilocal()`](https://rdrr.io/pkg/centiserve/man/semilocal.html).

## References

Chen, D., Lu, L., Shang, M.-S., Zhang, Y.-C., & Zhou, T. (2012).
Identifying influential nodes in complex networks. Physica A, 391(4),
1777-1787.
[doi:10.1016/j.physa.2011.09.017](https://doi.org/10.1016/j.physa.2011.09.017)
.

## See also

[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_semilocal(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        324        369        432        306        306        288        243 
#>   Evaluate     Create      Share 
#>        306        405        369 
```
