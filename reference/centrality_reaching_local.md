# Local Reaching Centrality

Local reaching centrality (Mones et al. 2012) measures how much of the
network a node reaches. On an unweighted directed network it is the
share of the other nodes reachable from the node. On an unweighted
undirected network it is the mean inverse distance to the other nodes,
harmonic centrality divided by \\n - 1\\.

## Usage

``` r
centrality_reaching_local(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

A network counts as weighted unless every weight is 1, and
`weighted = FALSE` gives the unweighted form. In the weighted form a
shortest path uses the edge lengths \\W/w_e\\, with \\W\\ the total edge
weight, and each reached node contributes the mean edge weight along its
path; the sum is divided by \\n - 1\\. With `mode = "out"` the weighted
values equal `networkx.local_reaching_centrality()` with
`normalized = False`. `mode = "all"` treats edges as undirected, and
`"in"` counts the nodes that reach the node. A negative weight raises an
error.
[`reaching_global`](https://sonsoles.me/cograph/reference/reaching_global.md)
is the network-level hierarchy measure built from these scores.

## References

Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for
complex networks. *PLoS ONE*, 7(3), e33799.

## See also

[`reaching_global`](https://sonsoles.me/cograph/reference/reaching_global.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_reaching_local(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.13055556 0.37277778 0.09703704 0.15333333 0.08703704 0.18851852 0.14333333 
#>   Evaluate     Create      Share 
#> 0.22722222 0.24592593 0.24444444 
```
