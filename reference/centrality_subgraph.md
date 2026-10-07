# Subgraph Centrality

Subgraph centrality (Estrada and Rodriguez-Velazquez 2005) counts the
closed walks that start and end at a node, weighting a walk of length
\\k\\ by \\1/k!\\: \$\$SC(i) = \sum\_{k=0}^{\infty}
\frac{(A^k)\_{ii}}{k!} = \left(e^{A}\right)\_{ii}.\$\$

## Usage

``` r
centrality_subgraph(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ is the binary adjacency matrix without self-loops, so edge weights
are ignored. On a directed network \\A\\ is replaced by \\A + A^{T}\\,
so a reciprocated pair has entry 2; the values then equal
[`igraph::subgraph_centrality()`](https://r.igraph.org/reference/subgraph_centrality.html).
An empty network raises an error.

## References

Estrada, E., & Rodriguez-Velazquez, J. A. (2005). Subgraph centrality in
complex networks. Physical Review E, 71(5), 056103.
[doi:10.1103/PhysRevE.71.056103](https://doi.org/10.1103/PhysRevE.71.056103)
.

## See also

[`centrality_communicability`](https://sonsoles.me/cograph/reference/centrality_communicability.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_subgraph(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   45.25014   64.11032   82.01339   42.15599   41.70540   34.34402   24.23952 
#>   Evaluate     Create      Share 
#>   38.64971   71.07596   57.03478 
```
