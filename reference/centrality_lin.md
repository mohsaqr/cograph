# Lin Centrality

Lin centrality divides the squared number of nodes a node reaches by the
sum of its distances to them: \$\$L(i) = \frac{r_i^2}{\sum\_{j \in R_i}
d\_{ij}},\$\$ where \\R_i\\ is the set of \\r_i\\ nodes reachable from
\\i\\. The score is defined on disconnected networks.

## Usage

``` r
centrality_lin(x, mode = "all", ...)
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
distances from the node and `"in"` distances to it. A node that reaches
no other node scores 0, and a single-node network gives `NA`. On
undirected networks the values equal
[`centiserve::lincent()`](https://rdrr.io/pkg/centiserve/man/lincent.html).

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lin(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   41.75258   36.32287   46.28571   30.68182   42.18750   28.02768   39.13043 
#>   Evaluate     Create      Share 
#>   32.66129   42.63158   28.52113 
```
