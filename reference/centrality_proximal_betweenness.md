# Proximal Betweenness Centrality

Proximal betweenness (Brandes 2008, section 3.2) is the fraction of
shortest paths on which a node is the last intermediate node before the
destination (proximal source) or the first intermediate node after the
source (proximal target). Each reachable ordered pair of nodes
contributes one unit, divided equally among its shortest paths.

## Usage

``` r
centrality_proximal_betweenness(x, proximal_variant = "source", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- proximal_variant:

  `"source"` (default), `"target"`, `"sum"` or `"union"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Shortest paths are counted with unit edge lengths on the simple network,
so direction is kept and weights, loops and parallel edges are ignored.
Ordered pairs are counted on undirected networks as well, so the scores
are not halved. Endpoints are excluded, and paths with fewer than two
edges contribute nothing. On an undirected network the source and target
variants agree, and `"sum"` is twice either of them. The `"union"`
variant counts a node once when a two-edge path makes it both proximal
source and proximal target. Isolated nodes and every node of a complete
graph score zero.

## References

Brandes, U. (2008). On variants of shortest-path betweenness centrality
and their generic computation. Social Networks, 30, 136-145.
[doi:10.1016/j.socnet.2007.11.001](https://doi.org/10.1016/j.socnet.2007.11.001)
.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_stress`](https://sonsoles.me/cograph/reference/centrality_stress.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_proximal_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   4.600000   8.083333   6.733333  14.750000   2.500000   2.933333   4.233333 
#>   Evaluate     Create      Share 
#>   2.266667   6.833333   7.066667 
```
