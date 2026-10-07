# Centroid Value

The centroid value (Koschuetzki et al. 2005) compares a node with every
other node by the number of nodes each one is closer to. With
\\\gamma(v, u)\\ the number of nodes strictly closer to \\v\\ than to
\\u\\, \$\$CV(v) = \min\_{u} \left\[ \gamma(v, u) - \gamma(u, v)
\right\].\$\$ The minimum includes \\u = v\\, so the score is at most 0,
and values closer to 0 mark more central nodes.

## Usage

``` r
centrality_centroid(x, mode = "all", ...)
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
sets the direction of the paths.

## References

Koschuetzki, D., Lehmann, K. A., Peeters, L., Richter, S.,
Tenfelde-Podehl, D., & Zlotowski, O. (2005). Centrality indices. In U.
Brandes & T. Erlebach (Eds.), Network Analysis: Methodological
Foundations (pp. 16-61). Springer.
[doi:10.1007/978-3-540-31955-9_3](https://doi.org/10.1007/978-3-540-31955-9_3)
.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_closeness_vitality`](https://sonsoles.me/cograph/reference/centrality_closeness_vitality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_centroid(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         -2         -6         -2         -8         -2         -8         -4 
#>   Evaluate     Create      Share 
#>         -8         -2         -8 
```
