# Closeness Vitality

Closeness vitality (Koschuetzki et al. 2005) is the drop in the Wiener
index when a node is removed: \$\$CV(v) = W(G) - W(G - v), \qquad W(G) =
\sum\_{s \ne t} d(s, t).\$\$ The Wiener index sums the finite distances
over ordered pairs, as in `networkx::closeness_vitality()`.

## Usage

``` r
centrality_closeness_vitality(x, mode = "all", ...)
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
sets the direction of the paths. Pairs that are not connected contribute
nothing to either index. An isolated node scores 0.

## References

Koschuetzki, D., Lehmann, K. A., Peeters, L., Richter, S.,
Tenfelde-Podehl, D., & Zlotowski, O. (2005). Centrality indices. In U.
Brandes & T. Erlebach (Eds.), Network Analysis: Methodological
Foundations (pp. 16-61). Springer.
[doi:10.1007/978-3-540-31955-9_3](https://doi.org/10.1007/978-3-540-31955-9_3)
.

## See also

[`centrality_wiener`](https://sonsoles.me/cograph/reference/centrality_wiener.md),
[`centrality_centroid`](https://sonsoles.me/cograph/reference/centrality_centroid.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_closeness_vitality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       2.60       4.04       1.34       5.28       0.82       5.78       4.12 
#>   Evaluate     Create      Share 
#>       4.96       2.52       5.68 
```
