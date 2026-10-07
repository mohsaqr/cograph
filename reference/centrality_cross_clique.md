# Cross-Clique Connectivity

Cross-clique connectivity (Faghani and Nguyen 2013) counts the cliques
that contain a node. Every complete subnetwork counts, including the
node itself and each of its edges, so a node of an isolated triangle
scores 4.

## Usage

``` r
centrality_cross_clique(x, ...)
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

A named integer vector with one count per node, in input node order.

## Details

Edge direction, edge weights and self-loops are ignored.

## References

Faghani, M. R., & Nguyen, U. T. (2013). A study of XSS worm propagation
and detection mechanisms in online social networks. IEEE Transactions on
Information Forensics and Security, 8(11), 1815-1826.
[doi:10.1109/TIFS.2013.2280884](https://doi.org/10.1109/TIFS.2013.2280884)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_cross_clique(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         11         16         21         12          9         10          8 
#>   Evaluate     Create      Share 
#>         12         17         13 
```
