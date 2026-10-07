# Domain Proximity Prestige

Domain proximity prestige (Wasserman and Faust 1994) combines the number
of nodes that reach a node with their distance to it: \$\$PD(v) =
\frac{R_v^2}{(n - 1)\\D_v},\$\$ where \\R_v\\ is the number of other
nodes with a directed path to \\v\\ and \\D_v\\ the sum of their hop
distances to \\v\\.

## Usage

``` r
centrality_prestige_domain_proximity(x, ...)
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

The measure needs a directed network. On undirected input every score is
`NA` with a warning that carries no condition class. Edge weights are
ignored. A node that no other node reaches scores 0, and the score lies
between 0 and 1. On strongly connected networks the values equal
`sna::prestige(cmode = "domain.proximity")`. On other networks sna sets
some scores to 0, because its sum multiplies an infinite distance by
zero; cograph sums the finite distances only.

## References

Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and
Applications*. Cambridge University Press.

## See also

[`centrality_prestige_domain`](https://sonsoles.me/cograph/reference/centrality_prestige_domain.md),
[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_prestige_domain_proximity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6428571  0.4500000  0.7500000  0.5625000  0.5625000  0.4736842  0.3913043 
#>   Evaluate     Create      Share 
#>  0.4736842  0.5625000  0.5625000 
```
