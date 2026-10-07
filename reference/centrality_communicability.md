# Communicability Centrality

Total communicability (Estrada and Hatano 2008; Benzi and Klymko 2013)
sums the walks of every length that start at a node, a walk of length
\\k\\ weighted by \\1/k!\\: \$\$TC(v) = \sum\_{w} \left\[ e^{A}
\right\]\_{vw} = \sum\_{w} \sum\_{k = 0}^{\infty}
\frac{(A^k)\_{vw}}{k!}.\$\$

## Usage

``` r
centrality_communicability(x, ...)
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
matrix exponential is formed from an eigendecomposition that assumes a
symmetric matrix. On a directed network the result therefore differs
from the row sums of \\e^{A}\\, and `directed = FALSE` gives the
undirected reading for which the measure is defined.

## References

Estrada, E., & Hatano, N. (2008). Communicability in complex networks.
Physical Review E, 77(3), 036111.
[doi:10.1103/PhysRevE.77.036111](https://doi.org/10.1103/PhysRevE.77.036111)
.

Benzi, M., & Klymko, C. (2013). Total communicability as a centrality
measure. Journal of Complex Networks, 1(2), 124-149.
[doi:10.1093/comnet/cnt007](https://doi.org/10.1093/comnet/cnt007) .

## See also

[`centrality_subgraph`](https://sonsoles.me/cograph/reference/centrality_subgraph.md),
[`centrality_communicability_betweenness`](https://sonsoles.me/cograph/reference/centrality_communicability_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_communicability(regulation_net, directed = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   218.0164   264.8024   302.7023   255.7658   212.7336   222.1101   188.5025 
#>   Evaluate     Create      Share 
#>   237.1442   269.3303   238.0564 
```
