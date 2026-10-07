# Decay Centrality

Decay centrality sums a decay factor \\\delta\\ raised to the distance
from the node to every node: \$\$D(v) = \sum\_{w} \delta^{d(v, w)}.\$\$
The sum includes the node itself, which adds one to every score, and
unreachable nodes contribute 0. Values of \\\delta\\ between 0 and 1
discount distant nodes.

## Usage

``` r
centrality_decay(x, mode = "all", decay_parameter = 0.5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- decay_parameter:

  Decay factor \\\delta\\. Default 0.5.

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
sets the direction of the paths. `decay_parameter` must lie strictly
between 0 and 1, and other values raise a `cograph_bad_parameter` error.
[`centrality_generalized_closeness`](https://sonsoles.me/cograph/reference/centrality_generalized_closeness.md)
computes the same quantity, and `decay_parameter = 0.5` gives
[`centrality_dangalchev`](https://sonsoles.me/cograph/reference/centrality_dangalchev.md).

## See also

[`centrality_generalized_closeness`](https://sonsoles.me/cograph/reference/centrality_generalized_closeness.md),
[`centrality_dangalchev`](https://sonsoles.me/cograph/reference/centrality_dangalchev.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_decay(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   8.765551   8.592851   8.877387   8.354837   8.781835   8.213449   8.690109 
#>   Evaluate     Create      Share 
#>   8.458391   8.781042   8.238714 
```
