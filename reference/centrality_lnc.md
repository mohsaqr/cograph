# Local Neighbor Contribution Centrality

Local neighbor contribution (Dai et al. 2019) multiplies a node's own
contribution \\d_i (1 - 1/d_i)^{d_i - 1}\\ by its neighbor contribution,
the squared degree times the neighbors' degree sum divided by \\n - 1\\.
\$\$LNC_i = d_i^3 \left(1 - \frac{1}{d_i}\right)^{d_i - 1}
\frac{\sum\_{j \in N(i)} d_j}{n - 1}\$\$

## Usage

``` r
centrality_lnc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored,
and it takes no parameters. The count \\n\\ is the number of nodes in
the whole network, so adding a disconnected component rescales every
score and leaves the ranking unchanged. Isolates and the node of a
single-node graph score zero. The factorization above is the one that
reproduces the source's printed intermediates and Table 1. The
Centrality Zoo (section 2.238) replaces the degree by the two-hop
neighborhood size and does not reproduce the source's values.

## References

Dai, J., Wang, B., Sheng, J., Sun, Z., Khawaja, F. R., Ullah, A.,
Dejene, D. A. and Duan, G. (2019). Identifying influential nodes in
complex networks based on local neighbor contribution. IEEE Access, 7,
131719-131731.
[doi:10.1109/ACCESS.2019.2939804](https://doi.org/10.1109/ACCESS.2019.2939804)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lnc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   153.6000   308.6420   559.2070   298.9969   147.9111   159.2889    72.0000 
#>   Evaluate     Create      Share 
#>   170.6667   318.2870   170.6667 
```
