# Average Distance

Average distance is the mean shortest-path distance from a node to the
other nodes, the definition the centiserve package states: \$\$AD(v) =
\frac{1}{n - 1} \sum\_{w \ne v} d(v, w).\$\$ (centiserve's code, and
cograph 2.7.19 and earlier, divide by \\n + 1\\.) Lower values mark more
central nodes.

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
#>  0.2155556  0.2477778  0.1944444  0.2933333  0.2133333  0.3211111  0.2300000 
#>   Evaluate     Create      Share 
#>  0.2755556  0.2111111  0.3155556 
```
