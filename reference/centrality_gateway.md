# Gateway Coefficient

The gateway coefficient (Ruiz Vargas and Wahl 2014) refines the
participation coefficient by weighting the links of node \\i\\ into each
module \\s\\ by how much of the connection between the two modules they
carry and by the degree of the neighbors they reach: \$\$G_i = 1 -
\frac{1}{k_i^2} \sum\_{s} k\_{is}^2 \\ g\_{is}^2,\$\$ where \\k\_{is}\\
is the number of links of \\i\\ into module \\s\\ and \\g\_{is}\\ lies
between 0 and 1.

## Usage

``` r
centrality_gateway(x, membership = NULL, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- membership:

  Module labels, one per node: integer codes, character labels or a
  factor.

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored, and the score lies between 0 and 1. On a
directed network `mode` chooses the ties: `"out"` uses outgoing links,
`"in"` incoming links and `"all"` (default) both, with a reciprocated
tie counted twice. The degree \\k_i\\, the module links \\k\_{is}\\ and
the neighbors whose degrees enter \\g\_{is}\\ all use the same ties. On
an undirected network the three modes agree and equal
`brainGraph::gateway_coeff(centr = "degree")`. `membership` can hold
integer, character or factor labels. Without `membership` the function
returns `NA` with a warning of classes `cograph_bad_membership` and
`cograph_undefined_measure`, and a `membership` of the wrong length
raises a `cograph_bad_membership` error. With a single module every node
scores 0, and so does a node without links in the chosen mode.

## References

Ruiz Vargas, E., & Wahl, L. M. (2014). The gateway coefficient: A novel
metric for identifying critical connections in modular networks. The
European Physical Journal B, 87(7), 161.
[doi:10.1140/epjb/e2014-40800-7](https://doi.org/10.1140/epjb/e2014-40800-7)
.

## See also

[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md),
[`centrality_within_module_z`](https://sonsoles.me/cograph/reference/centrality_within_module_z.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_gateway(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5808315  0.5490589  0.6204854  0.5562922  0.5796028  0.4941561  0.2874009 
#>   Evaluate     Create      Share 
#>  0.5067149  0.6424167  0.5239164 
```
