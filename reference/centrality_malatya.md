# Malatya centrality

The static Malatya score of a node is the sum of its degree divided by
each neighbour's degree: \\M(i)=\sum\_{j\in N(i)}d_i/d_j\\. Computes the
score on the original graph. On nonisolated vertices it is exactly the
reciprocal of
[`centrality_bridging_coefficient`](https://sonsoles.me/cograph/reference/centrality_truss.md);
this relationship follows from their definitions, not rank correlation.

## Usage

``` r
centrality_malatya(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive scores are divided by their
  maximum.

## Value

Named numeric vector in input node order.

## Details

Uses the simple undirected unweighted skeleton: either direction creates
an edge, parallel edges count once and self-loops are removed. This is
an explicit projection of other inputs to the source's domain. The empty
neighbour sum assigns isolates zero. On a regular graph the score equals
degree. High scores favour nodes with many neighbours of low degree.

## References

Karci, A., Yakut, S., & Oztemiz, F. (2022). A New Approach Based on
Centrality Value in Solving the Minimum Vertex Cover Problem: Malatya
Centrality Algorithm. Journal of Computer Science, 7(2), 81-88,
equation 1.
[doi:10.53070/bbd.1195501](https://doi.org/10.53070/bbd.1195501) .

## Examples

``` r
centrality_malatya(igraph::make_star(5, mode = "undirected"))
#>     1     2     3     4     5 
#> 16.00  0.25  0.25  0.25  0.25 
```
