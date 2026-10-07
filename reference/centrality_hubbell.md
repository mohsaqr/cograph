# Hubbell Centrality

Hubbell (1965) centrality solves an input-output system in which the
score of a node is one plus the attenuated scores of the nodes it sends
ties to: \$\$c = (I - wW)^{-1}\mathbf{1},\$\$ where \\W\\ is the
weighted adjacency matrix and \\w\\ is `hubbell_weight`.

## Usage

``` r
centrality_hubbell(x, hubbell_weight = 0.5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- hubbell_weight:

  Attenuation factor \\w\\, a positive number (default 0.5).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The system is solvable when the spectral radius of \\wW\\ is below one.
Otherwise every score is `NA` with a warning that carries no condition
class. A `hubbell_weight` of zero or below raises an error. Edge weights
are always used, and `weighted = FALSE` has no effect. The rows of \\W\\
are outgoing ties, so on a directed network the score sums attenuated
walks that leave the node.
[`centiserve::hubbell()`](https://rdrr.io/pkg/centiserve/man/hubbell.html)
with `weights = NULL` sets every weight to 1, so it reproduces these
values only when the weights are passed explicitly.

## References

Hubbell, C. H. (1965). An input-output approach to clique
identification. *Sociometry*, 28(4), 377-399.

## See also

[`centrality_katz`](https://sonsoles.me/cograph/reference/centrality_katz.md),
[`centrality_power`](https://sonsoles.me/cograph/reference/centrality_power.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_hubbell(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.458158   2.319798   1.452777   1.586972   1.145412   1.542454   1.418973 
#>   Evaluate     Create      Share 
#>   1.620997   1.761183   1.908969 
```
