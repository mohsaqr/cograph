# Node Contraction Centrality (IMC and IIMC)

Tan, Wu and Deng's (2006) node-contraction importance, as restated by
Wang et al. (2011). The agglomeration (cohesion) of a graph is
\\\partial(G) = 1 / (N \bar{L})\\, with \\\bar{L}\\ the mean
shortest-path length over ordered pairs; contracting a node merges it
with all its neighbours into one node, and \$\$IMC(v) = 1 - \partial(G)
/ \partial(G_v).\$\$ The improved form (`node_contraction_improved`)
adds the same score of the node's edges computed on the line graph:
\\IIMC(v) = \alpha\\ IMC(v) + \beta \sum\_{e \ni v} IMC\_{L(G)}(e)\\,
with \\\alpha / \beta = 5\\ (`contraction_rho`) and \\\alpha + \beta =
1\\, the normalisation that reproduces the paper's Table 1. Higher =
more important. Both reproduce Table 1 of Wang et al. (2011). The Zoo
entry describes the contracted graph as the graph with the node removed;
the sources define it by contraction, which is what is implemented.

## Usage

``` r
centrality_node_contraction(x, ...)

centrality_node_contraction_improved(x, contraction_rho = 5, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- contraction_rho:

  Ratio \\\alpha / \beta\\ for the improved form. Default 5.

## Value

Named numeric vector, one value per node.

## Details

On a disconnected graph the mean path length is taken over the mutually
reachable ordered pairs (a cograph choice; the sources assume connected
graphs). Direction, weights and loops are ignored. Cost is one all-pairs
computation per node, so \\O(n^2 (n + m))\\; the improved form does the
same on the line graph, \\O(m^2 (m + m'))\\.

## References

Tan, Y.-J., Wu, J., & Deng, H.-Z. (2006). Evaluation method for node
importance based on node contraction in complex networks. Systems
Engineering: Theory & Practice, 26(11), 79-83.

Wang, J., Li, C., & Xia, C. (2011). Improved centrality indicators to
characterize the nodal spreading capability in complex networks.
Procedia Engineering, 15, 3304-3308.

## See also

[`centrality_closeness_vitality`](https://sonsoles.me/cograph/reference/centrality_closeness_vitality.md).

## Examples

``` r
path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]
centrality_node_contraction(path5)
#>         A         B         C         D         E 
#> 0.3333333 0.6000000 0.6000000 0.6000000 0.3333333 
centrality_node_contraction_improved(path5)
#>         A         B         C         D         E 
#> 0.3444444 0.6833333 0.7333333 0.6833333 0.3444444 
```
