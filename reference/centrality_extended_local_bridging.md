# Extended Local Bridging Centrality

Extended local bridging centrality (Macker 2016) multiplies a node's
betweenness in its two-hop ego network by its bridging coefficient, the
reciprocal of its degree divided by the sum of the reciprocal degrees of
its neighbors. The two-hop ego network contains every node within two
hops and every edge among them, so its shortest paths can have up to
four edges.

## Usage

``` r
centrality_extended_local_bridging(x, ...)
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
unordered pairs, excludes endpoints and is not normalized. Isolated
nodes, leaves and every node of a complete graph score zero. The
weighted model of Macker (2016), with link quality and path costs, is
not implemented.

## References

Macker, J. P. (2016). An improved local bridging centrality model for
distributed network analytics. MILCOM, pp. 600-605.
[doi:10.1109/MILCOM.2016.7795393](https://doi.org/10.1109/MILCOM.2016.7795393)
.

## See also

[`centrality_localized_bridging`](https://sonsoles.me/cograph/reference/centrality_localized_bridging.md),
[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_extended_local_bridging(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3000000  0.3473648  0.3104056  0.4191617  0.4364508  0.3185185  0.2403169 
#>   Evaluate     Create      Share 
#>  0.2610169  0.2879113  0.2333333 
```
