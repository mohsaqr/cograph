# Barycenter Centrality

Barycenter centrality is the reciprocal of the total shortest-path
distance from a node to the nodes it reaches: \$\$BC(v) =
\frac{1}{\sum\_{w \ne v} d(v, w)}.\$\$ Unreachable nodes are left out of
the sum.

## Usage

``` r
centrality_barycenter(x, mode = "all", ...)
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
  exponent, default 1), `cutoff` (largest path length considered,
  default -1 for no limit) and `normalized` (divide by the maximum,
  default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as path lengths. `invert_weights = TRUE` uses
\\1/w^\alpha\\ as the length, and `weighted = FALSE` counts hops. `mode`
sets the direction of the paths. The formula is the one
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md)
computes, and with the default `weighted = TRUE` the two agree on every
node that reaches another node. A node that reaches no other node scores
0, where closeness returns `NaN`.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_average_distance`](https://sonsoles.me/cograph/reference/centrality_average_distance.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_barycenter(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5154639  0.4484305  0.5714286  0.3787879  0.5208333  0.3460208  0.4830918 
#>   Evaluate     Create      Share 
#>  0.4032258  0.5263158  0.3521127 
```
