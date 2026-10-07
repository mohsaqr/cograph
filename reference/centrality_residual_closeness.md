# Residual Closeness Centrality

Residual closeness (Dangalchev 2006) sums distances that decay by half
with every step: \$\$C(i) = \sum\_{j} 2^{-d\_{ij}},\$\$ with
\\2^{-\infty} = 0\\, so the score is defined on disconnected networks.

## Usage

``` r
centrality_residual_closeness(x, mode = "all", ...)
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

The sum includes the node itself, which adds 1, and the values equal
[`centiserve::closeness.residual()`](https://rdrr.io/pkg/centiserve/man/closeness.residual.html)
on undirected networks. Edge weights are read as distances;
`weighted = FALSE` uses hop counts, and `invert_weights = TRUE` converts
a weight \\w\\ to the distance \\1/w^\alpha\\. `mode = "all"` treats
edges as undirected, `"out"` uses distances from the node and `"in"`
distances to it.
[`centrality_dangalchev`](https://sonsoles.me/cograph/reference/centrality_dangalchev.md)
returns the same values.

## References

Dangalchev, C. (2006). Residual closeness in networks. Physica A,
365(2), 556-564.
[doi:10.1016/j.physa.2005.12.020](https://doi.org/10.1016/j.physa.2005.12.020)
.

## See also

[`centrality_dangalchev`](https://sonsoles.me/cograph/reference/centrality_dangalchev.md),
[`centrality_decay`](https://sonsoles.me/cograph/reference/centrality_decay.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_residual_closeness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   8.765551   8.592851   8.877387   8.354837   8.781835   8.213449   8.690109 
#>   Evaluate     Create      Share 
#>   8.458391   8.781042   8.238714 
```
