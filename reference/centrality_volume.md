# Volume Centrality

Volume centrality (Wehmuth and Ziviani 2011, 2013) is the sum of the
degrees of all nodes within `volume_radius` hops of a node, the node
itself included. Degrees are those of the whole network, so edges that
leave the neighborhood also count.

## Usage

``` r
centrality_volume(x, volume_radius = 2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- volume_radius:

  Hop radius: a nonnegative integer or `Inf`. Default 2.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored. Isolated nodes score 0. Radius 0
gives the degree, and an infinite radius gives twice the number of edges
in the node's component. A radius that is negative or not a whole number
raises an error.

## References

Wehmuth, K., & Ziviani, A. (2011). Distributed Assessment of Network
Centrality. arXiv:1108.1067.

Wehmuth, K., & Ziviani, A. (2013). DACCER: Distributed Assessment of the
Closeness CEntrality Ranking in complex networks. Computer Networks, 57,
2536-2548.
[doi:10.1016/j.comnet.2013.05.001](https://doi.org/10.1016/j.comnet.2013.05.001)
.

## See also

[`centrality_kreach`](https://sonsoles.me/cograph/reference/centrality_kreach.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_volume(regulation_net, volume_radius = 1)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         32         38         44         37         31         33         28 
#>   Evaluate     Create      Share 
#>         35         39         35 
```
