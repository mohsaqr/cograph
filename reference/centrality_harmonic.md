# Harmonic Centrality

Harmonic centrality (Marchiori and Latora 2000) sums the inverse
shortest-path distances from a node to the other nodes: \$\$H(i) =
\sum\_{j \ne i} \frac{1}{d\_{ij}},\$\$ with \\1/\infty = 0\\, so the
score is defined on disconnected networks (Boldi and Vigna 2014).
`centrality_inharmonic()` and `centrality_outharmonic()` are the
`mode = "in"` and `mode = "out"` forms.

## Usage

``` r
centrality_harmonic(x, mode = "all", ...)

centrality_inharmonic(x, ...)

centrality_outharmonic(x, ...)
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
  The measure uses `invert_weights` (default `NULL`, which inverts for
  tna input only), `alpha` (inversion exponent, default 1), `cutoff`
  (largest distance counted, default -1 for no limit) and `normalized`
  (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as distances, and `invert_weights = TRUE` converts
a weight \\w\\ to the distance \\1/w^\alpha\\. `weighted = FALSE` has no
effect, because the measure then reads the weights stored in the
network; hop-count harmonic centrality needs a binary input such as
`(x != 0) * 1`. `mode = "all"` treats edges as undirected, `"out"` uses
distances from the node and `"in"` distances to it. The scores equal
[`igraph::harmonic_centrality()`](https://r.igraph.org/reference/harmonic_centrality.html)
on the weighted graph. `normalized = TRUE` divides the scores by their
maximum.

## References

Marchiori, M., & Latora, V. (2000). Harmony in the small-world. Physica
A, 285(3-4), 539-546.
[doi:10.1016/S0378-4371(00)00311-3](https://doi.org/10.1016/S0378-4371%2800%2900311-3)
.

Boldi, P., & Vigna, S. (2014). Axioms for centrality. Internet
Mathematics, 10(3-4), 222-262.
[doi:10.1080/15427951.2013.865686](https://doi.org/10.1080/15427951.2013.865686)
.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_harmonic(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   58.05676   42.51660   56.48948   33.66818   63.71353   30.68703   50.87966 
#>   Evaluate     Create      Share 
#>   45.01176   45.82217   29.83552 
```
