# Volume centrality

Sum of the original-graph degrees of all vertices within `volume_radius`
hops, including the focal vertex. This is the localized volume measure
of Wehmuth & Ziviani (DANCE/DACCER). Degrees include edges leaving the
neighborhood; they are not recomputed inside the induced subgraph.
Radius zero returns degree. Infinite radius returns twice the number of
edges in the focal connected component.

## Usage

``` r
centrality_volume(x, volume_radius = 2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- volume_radius:

  Nonnegative integer hop radius, or `Inf`. Default 2, the local radius
  investigated by Wehmuth & Ziviani.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive scores are divided by their
  maximum.

## Value

Named numeric vector in input node order.

## Details

Uses the simple undirected, unweighted skeleton: either direction
creates an edge, parallel edges count once, and self-loops are removed.
This is an explicit input projection, not a weighted or directed
generalization. Isolates score zero.

## References

Wehmuth, K., & Ziviani, A. (2011). Distributed Assessment of Network
Centrality. Section II-A, equation 1.
<https://arxiv.org/abs/1108.1067v1>.

Wehmuth, K., & Ziviani, A. (2013). DACCER: Distributed Assessment of the
Closeness CEntrality Ranking in complex networks. Computer Networks, 57,
2536-2548.
[doi:10.1016/j.comnet.2013.05.001](https://doi.org/10.1016/j.comnet.2013.05.001)
.

## See also

[`centrality_kreach`](https://sonsoles.me/cograph/reference/centrality_kreach.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md).

## Examples

``` r
centrality_volume(igraph::make_ring(6), volume_radius = 1)
#> 1 2 3 4 5 6 
#> 6 6 6 6 6 6 
centrality_volume(igraph::make_ring(6), volume_radius = 0)
#> 1 2 3 4 5 6 
#> 2 2 2 2 2 2 
```
