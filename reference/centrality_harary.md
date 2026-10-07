# Harary Centrality

Harary centrality sums the inverse squared shortest-path distances from
a node to the other nodes: \$\$H(i) = \sum\_{j \ne i}
\frac{1}{d\_{ij}^{2}},\$\$ with \\1/\infty = 0\\, so an unreachable node
contributes nothing and the score is defined on disconnected networks.

## Usage

``` r
centrality_harary(x, mode = "all", ...)
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

Edge weights are read as distances. On `regulation_net`, whose weights
lie below one, the scores are therefore large. `weighted = FALSE` uses
hop counts, and `invert_weights = TRUE` converts a weight \\w\\ to the
distance \\1/w^\alpha\\. `mode = "all"` treats edges as undirected,
`"out"` uses distances from the node and `"in"` distances to it. The
Harary index of Plavsic et al. (1993) sums the inverse distances
\\1/d\_{ij}\\; its node-level form is
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md).

## References

Plavsic, D., Nikolic, S., Trinajstic, N., & Mihalic, Z. (1993). On the
Harary index for the characterization of chemical graphs. Journal of
Mathematical Chemistry, 12(1), 235-250.
[doi:10.1007/BF01164638](https://doi.org/10.1007/BF01164638) .

## See also

[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_harary(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   607.5926   240.9987   446.0798   140.9137   738.9974   121.1449   401.6495 
#>   Evaluate     Create      Share 
#>   348.7838   249.5841   103.9324 
```
