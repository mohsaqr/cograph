# Resistance Curvature

The node resistance curvature of Devriendt and Lambiotte (2022) is
\$\$p_i = 1 - \frac{1}{2} \sum\_{j \sim i} w\_{ij} R\_{ij},\$\$ where
the weights \\w\_{ij}\\ are conductances and \\R\_{ij}\\ is the
effective resistance. It equals one minus half the expected degree of
the node in a random spanning tree of its component.

## Usage

``` r
centrality_resistance_curvature(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`) and `normalized` (default
  `FALSE`), which divides by the largest score and leaves negative
  scores negative.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are conductances, and `weighted = FALSE` uses the simple
undirected skeleton. On a weighted directed network the two arcs between
a pair are added. Self-loops are removed, `mode` has no effect, and
negative or non-finite weights raise an error. Scores can be negative at
tree-like junctions. On a tree the score is one minus half the degree,
on an unweighted cycle or complete graph of \\n\\ nodes it is \\1/n\\,
and an isolated node scores 1. The raw scores sum to the number of
components.

## References

Devriendt, K., & Lambiotte, R. (2022). Discrete curvature on graphs from
the effective resistance. Journal of Physics: Complexity, 3, 025008.
[doi:10.1088/2632-072X/ac730d](https://doi.org/10.1088/2632-072X/ac730d)
.

## See also

[`centrality_current_flow_closeness`](https://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md),
[`centrality_dynamical_importance`](https://sonsoles.me/cograph/reference/centrality_dynamical_importance.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_resistance_curvature(regulation_net)
#>     Explore        Plan     Monitor       Adapt     Reflect     Discuss 
#>  0.14703607  0.01598811  0.03846915 -0.01385168  0.04645520  0.10397189 
#>  Synthesize    Evaluate      Create       Share 
#>  0.33242747  0.13041545  0.13930439  0.05978396 
```
