# Percolation Centrality

Percolation centrality (Piraveenan et al. 2013) weights each shortest
path through a node by the percolation state \\x_s\\ of its source:
\$\$PC(v) = \frac{1}{n-2} \sum\_{s \ne v \ne t}
\frac{\sigma\_{st}(v)}{\sigma\_{st}} \frac{x_s}{\sum_i x_i - x_v},\$\$
where \\\sigma\_{st}\\ is the number of shortest paths from \\s\\ to
\\t\\ and \\\sigma\_{st}(v)\\ the number through \\v\\. With equal
states the score is the normalized betweenness.

## Usage

``` r
centrality_percolation(x, states = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- states:

  Percolation state of each node, between 0 and 1. The default `NULL`
  gives every node state 1.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`), `invert_weights`
  (default `NULL`, which inverts for tna input only) and `alpha`
  (inversion exponent, default 1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as distances, `weighted = FALSE` uses hop counts,
and `invert_weights = TRUE` uses the distance \\1/w^\alpha\\, so tna
input is inverted by default. Paths follow edge direction on a directed
network. `states` is matched to nodes by name when it has names and by
position otherwise. Its values are clipped to \\\[0, 1\]\\ and missing
values are set to 1, and a vector of the wrong length raises an error. A
network with fewer than three nodes scores 0.

## References

Piraveenan, M., Prokopenko, M., & Hossain, L. (2013). Percolation
centrality: Quantifying graph-theoretic impact of nodes during
percolation in networks. PLoS ONE, 8(1), e53095.
[doi:10.1371/journal.pone.0053095](https://doi.org/10.1371/journal.pone.0053095)
.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_load`](https://sonsoles.me/cograph/reference/centrality_load.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_percolation(regulation_net)
#>     Explore        Plan     Monitor       Adapt     Reflect     Discuss 
#> 0.069444444 0.215277778 0.250000000 0.208333333 0.138888889 0.006944444 
#>  Synthesize    Evaluate      Create       Share 
#> 0.090277778 0.041666667 0.180555556 0.125000000 
```
