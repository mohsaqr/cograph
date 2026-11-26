# Shell Clustering Coefficient

The shell clustering coefficient (SCC; Zareie et al. 2020) adds over a
node's neighbors one term that is large when their positions in the
network's peeling order differ and one that grows with the neighbor's
degree: \$\$SCC_i = \sum\_{j \in N(i)} \Bigl\[(2 - C\_{ij}) + \Bigl(2
\frac{k_j}{\max_l k_l} + 1\Bigr)\Bigr\],\$\$ with \\k\\ the degree and
\\C\_{ij}\\ the Pearson correlation of the shell vectors of \\i\\ and
\\j\\. The shell vector of a node counts its neighbors in each
hierarchy, the hierarchy of a node being the round in which it is
removed when every round removes all remaining nodes of the lowest
degree.

## Usage

``` r
centrality_shell_clustering(x, ...)
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
The hierarchies follow the text of the source's Section 3 and are the
values of
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md).
The source's Algorithm 1 says only "the k-shell algorithm"; the usual
peeling with a rising threshold gives other hierarchies on some networks
(3 of the 34 nodes of the karate club, 30 of the 62 of the dolphins
network). When a shell vector takes one value in every hierarchy, as
when every node of a regular network leaves in the first round, the
correlation is 0/0; it is taken as 0 (owner decision 2026-11-26). The
two brackets are added, as in the source's Algorithm 1. An isolated
node, and every node of a network without edges, scores 0. The source
prints no score; the implementation reproduces the hierarchies of the 17
nodes of its Fig. 1, its three printed shell vectors and its correlation
\\C\_{8,9} \approx 0.75\\.
[`centrality_ecrm`](https://sonsoles.me/cograph/reference/centrality_ecrm.md)
is the source's ranking measure built on this one. On four test networks
the score ranks nodes closest to `graph_regularization`,
`current_flow_closeness` and `information` (mean Kendall correlation
0.89); with the degree the correlation is 0.83 to 0.93.
`normalized = TRUE` divides the scores by their maximum.

## References

Zareie, A., Sheikhahmadi, A., Jalili, M. and Fasaei, M. S. K. (2020).
Finding influential nodes in social networks based on neighborhood
correlation coefficient. Knowledge-Based Systems, 194, 105580.
[doi:10.1016/j.knosys.2020.105580](https://doi.org/10.1016/j.knosys.2020.105580)
.

## See also

[`centrality_ecrm`](https://sonsoles.me/cograph/reference/centrality_ecrm.md),
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_shell_clustering(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   19.93160   23.39115   27.21679   25.68549   23.24985   20.54101   18.45164 
#>   Evaluate     Create      Share 
#>   22.14954   23.49699   19.79473 
```
