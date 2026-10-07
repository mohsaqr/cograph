# Dangalchev Closeness

Dangalchev closeness, the residual closeness of Dangalchev (2006), sums
an exponentially decaying function of the distance to every node:
\$\$D(v) = \sum\_{w} 2^{-d(v, w)}.\$\$ The sum includes the node itself,
which adds one to every score, as in the centiserve package. Unreachable
nodes contribute 0.

## Usage

``` r
centrality_dangalchev(x, mode = "all", ...)
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
sets the direction of the paths. The values equal those of
[`centrality_residual_closeness`](https://sonsoles.me/cograph/reference/centrality_residual_closeness.md)
and of
[`centrality_decay`](https://sonsoles.me/cograph/reference/centrality_decay.md)
with `decay_parameter = 0.5`.

## References

Dangalchev, C. (2006). Residual closeness in networks. Physica A,
365(2), 556-564.
[doi:10.1016/j.physa.2005.12.020](https://doi.org/10.1016/j.physa.2005.12.020)
.

## See also

[`centrality_residual_closeness`](https://sonsoles.me/cograph/reference/centrality_residual_closeness.md),
[`centrality_decay`](https://sonsoles.me/cograph/reference/centrality_decay.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dangalchev(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   8.765551   8.592851   8.877387   8.354837   8.781835   8.213449   8.690109 
#>   Evaluate     Create      Share 
#>   8.458391   8.781042   8.238714 
```
