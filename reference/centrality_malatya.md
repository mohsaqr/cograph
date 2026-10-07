# Malatya Centrality

The Malatya centrality of a node (Karci et al. 2022) is the sum of its
degree divided by the degree of each neighbor, \\M(i) = \sum\_{j \in
N(i)} d_i / d_j\\. High scores mark nodes with many neighbors of low
degree.

## Usage

``` r
centrality_malatya(x, ...)
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

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored. Isolated nodes score 0. On a
regular network the score equals the degree. On every node with at least
one neighbor it is the reciprocal of
[`centrality_bridging_coefficient`](https://sonsoles.me/cograph/reference/centrality_truss.md).

## References

Karci, A., Yakut, S., & Oztemiz, F. (2022). A New Approach Based on
Centrality Value in Solving the Minimum Vertex Cover Problem: Malatya
Centrality Algorithm. Journal of Computer Science, 7(2), 81-88.
[doi:10.53070/bbd.1195501](https://doi.org/10.53070/bbd.1195501) .

## See also

[`centrality_bridging_coefficient`](https://sonsoles.me/cograph/reference/centrality_truss.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_malatya(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   4.666667   6.957143   9.450000   7.157143   4.964286   4.500000   2.704762 
#>   Evaluate     Create      Share 
#>   4.214286   6.657143   4.214286 
```
