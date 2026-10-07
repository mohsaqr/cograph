# Random Walk Centrality

Random walk centrality is the inverse of the summed random-walk
distances from a node to the others: \$\$RW(i) = \left(\sum\_{j}
\frac{m\_{ij} + m\_{ji}}{2}\right)^{-1},\$\$ where \\m\_{ij}\\ is the
mean first passage time from \\i\\ to \\j\\ of a walk that moves to each
out-neighbor with equal probability.

## Usage

``` r
centrality_random_walk(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. The measure is defined for connected
undirected and strongly connected directed networks. On any other
network every score is `NA`, with a `cograph_undefined_measure` warning,
because some passage times are infinite. The passage times are
symmetrized before the sum, so the values differ from
[`tidygraph::centrality_random_walk()`](https://tidygraph.data-imaginist.com/reference/centrality.html),
which sums them unsymmetrized.

## See also

[`centrality_markov`](https://sonsoles.me/cograph/reference/centrality_markov.md),
[`centrality_current_flow_closeness`](https://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_random_walk(regulation_net)
#>     Explore        Plan     Monitor       Adapt     Reflect     Discuss 
#> 0.012216636 0.008002654 0.013333103 0.012968861 0.011194826 0.008290386 
#>  Synthesize    Evaluate      Create       Share 
#> 0.007141018 0.006606541 0.011334481 0.011743819 
```
