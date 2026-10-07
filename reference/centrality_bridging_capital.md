# Bridging Capital

Bridging capital (Jackson 2020, section 3.3) credits node \\i\\ with the
expected number of walks of length at most \\T\\ that are lost when one
entry \\P\_{ij}\\ of the transmission matrix is deleted, summed over
\\j\\ and weighted by source-destination values \\v\_{st}\\: \$\$Brid_i
= \sum_j \sum\_{s,t} v\_{st} \sum\_{h=1}^{T} \left\[P^h - (P - P\_{ij}
E\_{ij})^h\right\]\_{st}.\$\$

## Usage

``` r
centrality_bridging_capital(x, bridging_steps = 2, bridging_values = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- bridging_steps:

  Horizon \\T\\, a nonnegative integer. Default 2.

- bridging_values:

  Nonnegative n by n matrix of source-destination values \\v\\. `NULL`
  (default) sets every value to one. Row and column names, when present,
  are matched to the node names.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`) and
  `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are the transmission probabilities in \\P\\ and must lie
between zero and one, and an unweighted edge has probability one. A
weight above one raises an error. Direction and loops are kept, and the
two entries of an undirected edge are deleted separately. Walks may
repeat nodes and edges, and a walk that uses the deleted entry several
times is counted once. Isolated nodes score zero, and
`bridging_steps = 0` gives zero scores. The function computes the
expected walk count EInf of the source paper.

## References

Jackson, M. O. (2020). A typology of social capital and associated
network measures. Social Choice and Welfare, 54, 311-336.
[doi:10.1007/s00355-019-01189-3](https://doi.org/10.1007/s00355-019-01189-3)
.

## See also

[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md),
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_bridging_capital(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>     1.4617     3.4556     1.7107     2.1084     0.5485     1.7608     0.9969 
#>   Evaluate     Create      Share 
#>     2.0890     2.3416     2.9270 
```
