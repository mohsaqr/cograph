# SALSA Authority Centrality

SALSA (Lempel and Moran 2000) ranks authorities by a random walk that
alternates between following an edge backward and forward. The authority
score of a node is its entry in the stationary distribution of the chain
\$\$\tilde{A} = W_c^{T} W_r,\$\$ where \\W_r\\ and \\W_c\\ are the
row-normalized and column-normalized adjacency matrices.

## Usage

``` r
centrality_salsa(x, ...)
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

The measure needs a directed network. On undirected input every score is
`NA` with a `cograph_undefined_measure` warning. Edge weights are
ignored. The scores are scaled so that the largest is 1, and a node
without incoming edges scores 0.

## References

Lempel, R., & Moran, S. (2000). The stochastic approach for
link-structure analysis (SALSA) and the TKC effect. Computer Networks,
33(1-6), 387-401.
[doi:10.1016/S1389-1286(00)00034-7](https://doi.org/10.1016/S1389-1286%2800%2900034-7)
.

## See also

[`centrality_authority`](https://sonsoles.me/cograph/reference/centrality_authority.md),
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_salsa(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6666667  0.3333333  1.0000000  0.5000000  0.6666667  0.3333333  0.1666667 
#>   Evaluate     Create      Share 
#>  0.3333333  0.5000000  0.5000000 
```
