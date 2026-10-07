# Improved Closeness Centrality

Improved closeness (Luan et al. 2021) divides each hop distance by a
power of the number of shortest paths, so that a partner reached along
many shortest paths counts as closer: \$\$ICC(i) = \frac{n-1}{\sum\_{j
\ne i} d\_{ij} / \sigma\_{ij}^{\alpha}}.\$\$ Here \\d\_{ij}\\ is the hop
distance and \\\sigma\_{ij}\\ the number of shortest paths between \\i\\
and \\j\\.

## Usage

``` r
centrality_improved_closeness(x, icc_alpha = 0.2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- icc_alpha:

  Exponent \\\alpha\\ of the number of shortest paths, between 0 and 1.
  Default 0.2, one of the values studied by Luan et al. (2021).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored, and `mode` has no effect. With
\\\alpha = 0\\ the score on a connected network is the normalized
closeness, and on a tree it does not depend on \\\alpha\\. Scores can
exceed 1. On a disconnected network every node scores 0, because each
node has an unreachable partner at infinite distance. An isolated node
also scores 0. A value of `icc_alpha` outside 0 to 1 raises an error.

## References

Luan, Y., Bao, Z., & Zhang, H. (2021). Identifying Influential Spreaders
in Complex Networks by Considering the Impact of the Number of Shortest
Paths. Journal of Systems Science and Complexity, 34, 2168-2181.
[doi:10.1007/s11424-021-0111-7](https://doi.org/10.1007/s11424-021-0111-7)
.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_improved_closeness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7848073  0.8514053  0.8971799  0.8696763  0.8019894  0.8069843  0.7371681 
#>   Evaluate     Create      Share 
#>  0.8069843  0.8406205  0.7909990 
```
