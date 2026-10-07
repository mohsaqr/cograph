# Burt's Constraint

Burt's constraint measures how much a node's ties are concentrated in
contacts that are themselves tied to each other. With \\p\_{ij}\\ the
proportion of the tie strength of \\i\\ invested in \\j\\, \$\$C_i =
\sum\_{j \ne i} \Big( p\_{ij} + \sum\_{q \ne i, j} p\_{iq} p\_{qj}
\Big)^2.\$\$ Low constraint marks access to structural holes.

## Usage

``` r
centrality_constraint(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Ties are symmetrized as \\w\_{ij} + w\_{ji}\\ before the proportions are
formed, so edge direction is ignored. With `weighted = FALSE` every tie
has weight one. The values match
[`igraph::constraint()`](https://r.igraph.org/reference/constraint.html).
An isolated node returns `NaN`, and a node whose only tie is a self-loop
scores 0.

## See also

[`centrality_effective_size`](https://sonsoles.me/cograph/reference/centrality_effective_size.md),
[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_constraint(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3938089  0.3609471  0.4445897  0.2953747  0.3318307  0.3363414  0.4122426 
#>   Evaluate     Create      Share 
#>  0.3801232  0.4686083  0.3627823 
```
