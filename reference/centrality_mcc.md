# Maximal Clique Centrality

Maximal clique centrality (MCC; Chin et al. 2014) sums \\(\|C\|-1)!\\
over the maximal cliques \\C\\ that contain the node. A clique contained
in a larger clique does not count.

## Usage

``` r
centrality_mcc(x, ...)
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

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored. Single-node cliques are excluded,
so an isolated node scores 0. A node whose neighbors share no edge
scores its degree. Maximal clique enumeration has exponential worst-case
cost. A score beyond double precision, which includes any clique with
more than 171 nodes, raises an error.

## References

Chin, C. H., et al. (2014). cytoHubba: identifying hub objects and
sub-networks from complex interactome. BMC Systems Biology, 8(Suppl 4),
S11.
[doi:10.1186/1752-0509-8-S4-S11](https://doi.org/10.1186/1752-0509-8-S4-S11)
.

## See also

[`centrality_cross_clique`](https://sonsoles.me/cograph/reference/centrality_cross_clique.md),
[`centrality_epc`](https://sonsoles.me/cograph/reference/centrality_local_efficiency.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mcc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         10         16         24         10          6          8          6 
#>   Evaluate     Create      Share 
#>         10         18         12 
```
