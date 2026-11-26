# RMD-Weighted Degree

The RMD-weighted degree (WD; Yang et al. 2018) sums, over the neighbors
of a node, the round in which each neighbor is removed by the
remaining-minimum-degree decomposition, as a share of the number of
rounds: \$\$WD(v) = \sum\_{u \in N(v)} \frac{Iter(u)}{MaxIter}.\$\$ The
decomposition removes, in each round, every node whose current degree
equals the lowest current degree. A neighbor removed late lies deep in
the network and adds more to the score.

## Usage

``` r
centrality_wd(x, ...)
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
\\Iter\\ is the index
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md)
returns. Rounds are counted over the whole network, so the components of
a disconnected network share them. Isolated nodes score 0. The
implementation reproduces the worked value and the ranking the source
prints for its 15-node example. `normalized = TRUE` divides the scores
by their maximum.

## References

Yang, F., Li, X., Xu, Y., Liu, X., Wang, J., Zhang, Y., Zhang, R. and
Yao, Y. (2018). Ranking the spreading influence of nodes in complex
networks: An extended weighted degree centrality based on a remaining
minimum degree decomposition. Physics Letters A, 382(34), 2361-2371.
[doi:10.1016/j.physleta.2018.05.032](https://doi.org/10.1016/j.physleta.2018.05.032)
.

## See also

[`centrality_ewd`](https://sonsoles.me/cograph/reference/centrality_ewd.md),
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wd(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        3.8        4.4        5.0        4.0        3.0        3.8        3.2 
#>   Evaluate     Create      Share 
#>        4.2        4.8        4.4 
```
