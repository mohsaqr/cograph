# Extended RMD-Weighted Degree

The extended RMD-weighted degree (EWD; Yang et al. 2018) sums the
RMD-weighted degree of the neighbors of a node: \$\$EWD(v) = \sum\_{u
\in N(v)} WD(u),\$\$ with \\WD\\ the score
[`centrality_wd`](https://sonsoles.me/cograph/reference/centrality_wd.md)
returns. It extends that score from the neighbors of a node to the nodes
two steps away.

## Usage

``` r
centrality_ewd(x, ...)
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

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The sum for a neighbor includes the node itself, as the worked value in
the source requires, so a node is counted once for each of its
neighbors. Isolated nodes score 0. The implementation reproduces the
worked value and the ranking the source prints for its 15-node example.
`normalized = TRUE` divides the scores by their maximum.

## References

Yang, F., Li, X., Xu, Y., Liu, X., Wang, J., Zhang, Y., Zhang, R. and
Yao, Y. (2018). Ranking the spreading influence of nodes in complex
networks: An extended weighted degree centrality based on a remaining
minimum degree decomposition. Physics Letters A, 382(34), 2361-2371.
[doi:10.1016/j.physleta.2018.05.032](https://doi.org/10.1016/j.physleta.2018.05.032)
.

## See also

[`centrality_wd`](https://sonsoles.me/cograph/reference/centrality_wd.md),
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ewd(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       20.0       25.4       28.0       24.4       20.0       20.0       16.4 
#>   Evaluate     Create      Share 
#>       21.2       25.6       22.0 
```
