# Average Distance

Average distance is the sum of the shortest-path distances from a node
to every node, divided by \\n + 1\\ as in the centiserve package:
\$\$AD(v) = \frac{1}{n + 1} \sum\_{w} d(v, w).\$\$ Lower values mark
more central nodes.

## Usage

``` r
centrality_average_distance(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`), `invert_weights`
  (default `NULL`, which is `TRUE` for tna input), `alpha` (inversion
  exponent, default 1) and `cutoff` (largest path length considered,
  default -1 for no limit).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as path lengths. `invert_weights = TRUE` uses
\\1/w^\alpha\\ as the length, and `weighted = FALSE` counts hops. `mode`
sets the direction of the paths. A node that cannot reach every other
node scores `Inf`, so on a disconnected network every score is `Inf`.

## See also

[`centrality_barycenter`](https://sonsoles.me/cograph/reference/centrality_barycenter.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_average_distance(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.1763636  0.2027273  0.1590909  0.2400000  0.1745455  0.2627273  0.1881818 
#>   Evaluate     Create      Share 
#>  0.2254545  0.1727273  0.2581818 
```
