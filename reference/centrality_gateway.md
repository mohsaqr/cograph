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

  Integer module codes, one per node.

- mode:

  Accepted for a uniform interface. It has no effect. Default `"all"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. On an undirected network the score lies
between 0 and 1. On a directed network \\k_i\\ is the in-degree while
\\k\_{is}\\ counts outgoing links, and the score can be negative, as on
`regulation_net`. `membership` must hold integer codes `1, ..., m`.
Without `membership` the function raises an unclassed warning and
returns `NA`, and a `membership` of the wrong length or with character
labels raises an unclassed error. With a single module every node scores
0, and so does a node without incoming links.

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
#>  0.8864840 -2.4522161  0.9504836  0.5123884  0.7838566 -0.1706674 -7.3560786 
#>   Evaluate     Create      Share 
#> -1.1420159  0.2270155  0.1404383 
```
