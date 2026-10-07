# Localized Bridging Centrality

Localized bridging centrality (Nanda and Kotz 2012) is the product of a
node's betweenness \\B^{ego}\_i\\ in its one-hop ego network and its
bridging coefficient, the reciprocal of its degree divided by the sum of
the reciprocal degrees of its neighbors: \$\$LBC_i = B^{ego}\_i \\
\frac{1/d_i}{\sum\_{j \in N(i)} 1/d_j}.\$\$

## Usage

``` r
centrality_localized_bridging(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Degrees are taken from the whole network. Ego betweenness counts
unordered pairs of the other ego-network nodes, excludes endpoints and
is not normalized. Isolated nodes, leaves and every node of a complete
graph score zero.

## References

Nanda, S. and Kotz, D. (2012). Localized Bridging Centrality. In
Handbook of Optimization in Complex Networks, pp. 197-224.
[doi:10.1007/978-1-4614-0857-4_7](https://doi.org/10.1007/978-1-4614-0857-4_7)
.

## See also

[`centrality_extended_local_bridging`](https://sonsoles.me/cograph/reference/centrality_extended_local_bridging.md),
[`centrality_local_bridging`](https://sonsoles.me/cograph/reference/centrality_local_bridging.md),
[`centrality_ego_betweenness`](https://sonsoles.me/cograph/reference/centrality_length_scaled_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_localized_bridging(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5714286  0.6228611  0.4320988  1.0479042  1.3093525  0.8888889  0.5545775 
#>   Evaluate     Create      Share 
#>  0.5932203  0.5257511  0.3954802 
```
