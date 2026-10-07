# Closeness Centrality

Closeness centrality (Sabidussi 1966) is the reciprocal of the total
shortest-path distance from a node to the nodes it reaches: \$\$C(v) =
\frac{1}{\sum\_{w \ne v} d(v, w)}.\$\$ Unreachable nodes are left out of
the sum, as in
[`igraph::closeness()`](https://r.igraph.org/reference/closeness.html).

## Usage

``` r
centrality_closeness(x, mode = "all", ...)

centrality_incloseness(x, ...)

centrality_outcloseness(x, ...)
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
  The measure uses `invert_weights` (default `NULL`, which is `TRUE` for
  tna input), `alpha` (inversion exponent, default 1), `cutoff` (largest
  path length considered, default -1 for no limit) and `normalized`
  (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as path lengths, and `invert_weights = TRUE` uses
\\1/w^\alpha\\ instead. `weighted = FALSE` uses hop counts.
`mode = "out"` follows paths leaving the node and `mode = "in"` paths
arriving at it. `centrality_outcloseness()` and
`centrality_incloseness()` are these two forms. A node that reaches no
other node returns `NaN`. `normalized = TRUE` multiplies each score by
the number of other nodes the node reaches, as igraph does.

## References

Sabidussi, G. (1966). The centrality index of a graph. Psychometrika,
31(4), 581-603.
[doi:10.1007/BF02289527](https://doi.org/10.1007/BF02289527) .

## See also

[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality_barycenter`](https://sonsoles.me/cograph/reference/centrality_barycenter.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_closeness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5154639  0.4484305  0.5714286  0.3787879  0.5208333  0.3460208  0.4830918 
#>   Evaluate     Create      Share 
#>  0.4032258  0.5263158  0.3521127 
```
