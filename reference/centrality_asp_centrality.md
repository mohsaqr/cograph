# Average Shortest Path Centrality

Average shortest path centrality (AC; Lv et al. 2019) is the relative
growth of the average distance of a network when the ties of a node are
removed: \$\$AC(k) = \frac{\|ASP(G'\_k) - ASP(G)\|}{ASP(G)}, \qquad
ASP(G) = \frac{1}{N(N - 1)} \sum\_{i \ne j} d\_{ij},\$\$ where \\G'\_k\\
is the network without the ties of node \\k\\, and a pair that cannot
reach each other counts the diameter of the original network.

## Usage

``` r
centrality_asp_centrality(x, ...)
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
Node \\k\\ stays in \\G'\_k\\ as an isolated node, so its own pairs
count the diameter and \\N(N - 1)\\ does not change: the source's worked
example computes it so, and its printed numbers, nine scores and two
averages, follow this reading and not the \\N - 1\\ nodes its prose
describes. The score is therefore never negative. On a disconnected
network the unreachable pairs of the network itself also count its
largest finite distance. An edgeless network and a one-node network
score 0. On a connected network without cut vertices the score is a
decreasing linear function of
[`centrality_closeness_vitality`](https://sonsoles.me/cograph/reference/centrality_closeness_vitality.md),
so the two rank nodes in exactly opposite order; they differ at cut
vertices, where the pairs that are cut off count the diameter. The score
ranks nodes almost like
[`centrality_exogenous`](https://sonsoles.me/cograph/reference/centrality_exogenous.md)
(Kendall correlation 0.997 on the karate club network, 0.94 to 0.95 on
three random networks). The implementation reproduces the nine scores
the source prints for its 9-node example, on the edge list read from its
figure. The distances are computed again for each node, so the cost is
that of the distances times the number of nodes. `normalized = TRUE`
divides the scores by their maximum.

## References

Lv, Z., Zhao, N., Xiong, F. and Chen, N. (2019). A novel measure of
identifying influential nodes in complex networks. Physica A:
Statistical Mechanics and its Applications, 523, 488-497.
[doi:10.1016/j.physa.2019.01.136](https://doi.org/10.1016/j.physa.2019.01.136)
.

## See also

[`centrality_closeness_vitality`](https://sonsoles.me/cograph/reference/centrality_closeness_vitality.md),
[`centrality_exogenous`](https://sonsoles.me/cograph/reference/centrality_exogenous.md),
[`centrality_average_distance`](https://sonsoles.me/cograph/reference/centrality_average_distance.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_asp_centrality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.07936508 0.09523810 0.11111111 0.09523810 0.07936508 0.07936508 0.06349206 
#>   Evaluate     Create      Share 
#> 0.07936508 0.09523810 0.07936508 
```
