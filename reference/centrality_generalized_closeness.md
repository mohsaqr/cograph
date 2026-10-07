# Generalized Closeness

Generalized closeness, as in the tidygraph package, sums a decay factor
\\\alpha\\ raised to the distance from the node to every node: \$\$GC(v)
= \sum\_{w} \alpha^{d(v, w)}.\$\$ The sum includes the node itself,
which adds one to every score, and unreachable nodes contribute 0.

## Usage

``` r
centrality_generalized_closeness(x, mode = "all", decay_parameter = 0.5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- decay_parameter:

  Decay factor \\\alpha\\ of the formula. Default 0.5.

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
\\1/w^\alpha\\ as the length, with the inversion exponent `alpha` of
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md), and
`weighted = FALSE` counts hops. `mode` sets the direction of the paths.
The values equal those of
[`centrality_decay`](https://sonsoles.me/cograph/reference/centrality_decay.md)
with the same `decay_parameter`, which is not checked.

## See also

[`centrality_decay`](https://sonsoles.me/cograph/reference/centrality_decay.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_generalized_closeness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   8.765551   8.592851   8.877387   8.354837   8.781835   8.213449   8.690109 
#>   Evaluate     Create      Share 
#>   8.458391   8.781042   8.238714 
```
