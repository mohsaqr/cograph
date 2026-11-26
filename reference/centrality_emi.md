# Entropy and Mutual Information Centrality

The entropy and mutual information-based centrality (EMI; Li et al.
2020) adds the entropy of the degrees around a node to a mutual
information term built on the neighbors it shares with each neighbor:
\$\$S_i = -\sum\_{j \in N_i} \frac{d_j}{EXD_i}
\log\_{10}\frac{d_j}{EXD_i}, \qquad MI_i = \sum\_{j \in N_i}
\frac{c\_{ij}}{d_i + d_j} \log\_{10}\frac{d_i + d_j - c\_{ij}}{d_i +
d_j},\$\$ and \\EMI_i = S_i + MI_i\\, with \\d\\ the degree, \\EXD_i =
\sum\_{j \in N_i} d_j\\ and \\c\_{ij}\\ the number of common neighbors
of \\i\\ and \\j\\.

## Usage

``` r
centrality_emi(x, ...)
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

This is the form of the source for undirected unweighted networks. It is
computed on the simple undirected skeleton, so direction, weights, loops
and parallel edges are ignored. The mutual information term is never
positive, and is zero for a node that shares no neighbor with any of its
neighbors. Isolated nodes and nodes of degree one score 0.
`normalized = TRUE` divides the scores by their maximum.

## References

Li, Y., Cai, W., Li, Y. and Du, X. (2020). Key node ranking in complex
networks: a novel entropy and mutual information-based approach.
Entropy, 22(1), 52.
[doi:10.3390/e22010052](https://doi.org/10.3390/e22010052) .

## See also

[`centrality_local_entropy`](https://sonsoles.me/cograph/reference/centrality_heatmap.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_emi(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5950828  0.6236162  0.5986123  0.7018123  0.6589627  0.6228680  0.5464759 
#>   Evaluate     Create      Share 
#>  0.5978192  0.6039034  0.5765194 
```
