# Randomized Shortest Paths Betweenness Centrality

Randomized shortest paths betweenness (Kivimaki et al. 2016) scores a
node by the expected number of visits it receives from absorbing walks
between every ordered source-target pair, under a Boltzmann distribution
that favors low-cost walks with inverse temperature \\\beta\\. Large
\\\beta\\ approaches shortest-path betweenness, and \\\beta \to 0\\
approaches a random-walk quantity that is proportional to degree on an
undirected graph. With \\Z = (I - W)^{-1}\\ and \\W = (D^{-1}A) \circ
\exp(-\beta C)\\: \$\$bet_i = \sum\_{s,t}
\left(\frac{z\_{si}}{z\_{st}} - \frac{z\_{ti}}{z\_{tt}}\right)
z\_{it}\$\$

## Usage

``` r
centrality_rsp_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `rsp_beta` (inverse temperature, default 0.01),
  `rsp_cost` (`"inverse"` (default) or `"weight"`) and `weighted` (use
  edge weights, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Direction and edge weights are used and loops are dropped. `mode`,
`cutoff` and `invert_weights` have no effect. The cost \\C\\ is \\1/w\\
with `rsp_cost = "inverse"` and \\w\\ with `"weight"`, and with
`weighted = FALSE` every arc costs one. Following the source, a pair
with no connecting path contributes zero, so scores are component-local,
and nodes with no out-edges score zero. A `rsp_beta` at or below zero
raises a `cograph_bad_parameter` error, and negative or non-finite
weights raise a `cograph_bad_input` error. The measure needs one dense
matrix inverse, so it is costly and is computed only when requested by
name or through `include`.

## References

Kivimaki, I., Lebichot, B., Saramaki, J. and Saerens, M. (2016). Two
betweenness centrality measures based on Randomized Shortest Paths.
Scientific Reports, 6, 19668.
[doi:10.1038/srep19668](https://doi.org/10.1038/srep19668) .

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_rsp_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   79.71481   22.23475  132.02683   91.88715   79.40471   43.82322   23.30359 
#>   Evaluate     Create      Share 
#>   51.38749  102.91337   69.90697 
```
