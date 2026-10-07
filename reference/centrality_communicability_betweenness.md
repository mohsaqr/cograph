# Communicability Betweenness

Communicability betweenness (Estrada, Higham and Hatano 2009) is the
share of the communicability between other pairs of nodes that is lost
when the node is removed. With \\G = e^{A}\\ and \\G^{(r)}\\ the same
exponential after the edges of \\r\\ are deleted, \$\$\omega_r =
\frac{1}{(n-1)(n-2)} \sum\_{p \ne q,\\ p, q \ne r} \frac{G\_{pq} -
G^{(r)}\_{pq}}{G\_{pq}}.\$\$

## Usage

``` r
centrality_communicability_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ is the binary adjacency matrix, so edge weights are ignored. The
scores lie between 0 and 1. The measure is defined for undirected
networks. On a directed network the matrix exponential needs the inverse
of an eigenvector matrix, and when that matrix is singular the function
stops with an unclassed error, as it does for `regulation_net`.
`directed = FALSE` gives the undirected reading.

## References

Estrada, E., Higham, D. J., & Hatano, N. (2009). Communicability
betweenness in complex networks. Physica A, 388(5), 764-774.
[doi:10.1016/j.physa.2008.11.011](https://doi.org/10.1016/j.physa.2008.11.011)
.

## See also

[`centrality_communicability`](https://sonsoles.me/cograph/reference/centrality_communicability.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_communicability_betweenness(regulation_net, directed = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3148847  0.4393980  0.5536645  0.4284950  0.3108637  0.3278741  0.2334877 
#>   Evaluate     Create      Share 
#>  0.3549724  0.4475289  0.3531491 
```
