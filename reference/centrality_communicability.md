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

\\A\\ is the binary adjacency matrix, so edge weights are ignored. On an
undirected network the matrix exponential is formed from the
eigendecomposition of the symmetric \\A\\. On a directed network it is
computed by scaling and squaring with a Pade approximation (Moler and
Van Loan 2003), and the scores are the row sums of \\e^{A}\\, the walks
that leave each node. `directed = FALSE` gives the undirected reading.

## References

Estrada, E., & Hatano, N. (2008). Communicability in complex networks.
Physical Review E, 77(3), 036111.
[doi:10.1103/PhysRevE.77.036111](https://doi.org/10.1103/PhysRevE.77.036111)
.

Benzi, M., & Klymko, C. (2013). Total communicability as a centrality
measure. Journal of Complex Networks, 1(2), 124-149.
[doi:10.1093/comnet/cnt007](https://doi.org/10.1093/comnet/cnt007) .

Moler, C., & Van Loan, C. (2003). Nineteen dubious ways to compute the
exponential of a matrix, twenty-five years later. SIAM Review, 45(1),
3-49.
[doi:10.1137/S00361445024180](https://doi.org/10.1137/S00361445024180) .

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
