# Weighted k-shell Degree Neighborhood Centrality

The weighted k-shell degree neighborhood method (WKSDN; Maji 2020) gives
every tie a weight built from the k-shell indices and degrees of its two
ends, and scores a node by the weights of its ties: \$\$WKSDN(i) =
\sum\_{j \in N(i)} \bigl\[(ks_i + ks_j) + \lambda (k_i + k_j)\bigr\],
\qquad \lambda = \frac{\sum_l ks_l}{\sum_l k_l}.\$\$

## Usage

``` r
centrality_wksdn(x, ...)
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
network, so direction, weights, loops and parallel edges are ignored.
\\\lambda\\ is taken over the whole network, so adding a component can
change every score. Isolated nodes score 0. The equations were taken
from a later paper that reproduces them and from the Centrality Zoo; the
2020 article was not consulted, and no published value was available to
check against. `normalized = TRUE` divides the scores by their maximum.

## References

Maji, G. (2020). Influential spreaders identification in complex
networks with potential edge weight based k-shell degree neighborhood
method. Journal of Computational Science, 39, 101055.
[doi:10.1016/j.jocs.2019.101055](https://doi.org/10.1016/j.jocs.2019.101055)
.

## See also

[`centrality_ink`](https://sonsoles.me/cograph/reference/centrality_ink.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wksdn(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   78.51852   98.37037  119.70370   97.62963   77.77778   79.25926   61.62963 
#>   Evaluate     Create      Share 
#>   80.74074   99.11111   80.74074 
```
