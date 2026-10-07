# Wiener Index Centrality

The Wiener centrality of a node is the sum of its shortest-path
distances to the nodes it reaches: \$\$W(i) = \sum\_{j \ne i,\\ d\_{ij}
\< \infty} d\_{ij}.\$\$ High values mark peripheral nodes. On a
connected undirected network half the sum of the scores is the Wiener
index of the network (Wiener 1947).

## Usage

``` r
centrality_wiener(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`), `invert_weights`
  (default `NULL`, which inverts for tna input only), `alpha` (inversion
  exponent, default 1) and `cutoff` (largest distance counted, default
  -1 for no limit).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as distances. `weighted = FALSE` uses hop counts,
and `invert_weights = TRUE` converts a weight \\w\\ to the distance
\\1/w^\alpha\\. `mode = "all"` treats edges as undirected, `"out"` uses
distances from the node and `"in"` distances to it. Unreachable nodes
add nothing, so on a disconnected network a node in a small component
also scores low.

## References

Wiener, H. (1947). Structural determination of paraffin boiling points.
Journal of the American Chemical Society, 69(1), 17-20.
[doi:10.1021/ja01193a005](https://doi.org/10.1021/ja01193a005) .

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_average_distance`](https://sonsoles.me/cograph/reference/centrality_average_distance.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wiener(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       1.94       2.23       1.75       2.64       1.92       2.89       2.07 
#>   Evaluate     Create      Share 
#>       2.48       1.90       2.84 
```
