# Node Contraction Centrality

Node contraction importance (Tan, Wu and Deng 2006, as restated by Wang
et al. 2011) compares the agglomeration \\\partial(G) = 1 / (N
\bar{L})\\ of a network, with \\\bar{L}\\ the mean shortest-path length,
before and after the node is merged with all its neighbors into one
node: \$\$IMC(v) = 1 - \frac{\partial(G)}{\partial(G_v)}.\$\$ The
improved form adds the same score of the edges of the node computed on
the line graph, \\IIMC(v) = \alpha\\ IMC(v) + \beta \sum\_{e \ni v}
IMC\_{L(G)}(e)\\, with \\\alpha + \beta = 1\\.

## Usage

``` r
centrality_node_contraction(x, ...)

centrality_node_contraction_improved(x, contraction_rho = 5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

- contraction_rho:

  Ratio \\\alpha / \beta\\ for the improved form (default 5).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights and loops are ignored. Higher values mark
more important nodes. The sources assume a connected network. On a
disconnected network the mean path length is taken over the mutually
reachable ordered pairs. An isolated node scores 0, and a node whose
contraction leaves no pair of mutually reachable nodes returns `NaN`.
The Centrality Zoo describes the contracted graph as the graph with the
node removed. The sources define it by contraction, which is implemented
here.

## References

Tan, Y.-J., Wu, J., & Deng, H.-Z. (2006). Evaluation method for node
importance based on node contraction in complex networks. Systems
Engineering: Theory & Practice, 26(11), 79-83.

## See also

[`centrality_closeness_vitality`](https://sonsoles.me/cograph/reference/centrality_closeness_vitality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_node_contraction(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6071429  0.6666667  0.7857143  0.6190476  0.5714286  0.5357143  0.4571429 
#>   Evaluate     Create      Share 
#>  0.5000000  0.6666667  0.5357143 
centrality_node_contraction_improved(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.8198386  0.9608238  1.1638901  0.9170453  0.7829129  0.7645413  0.6183757 
#>   Evaluate     Create      Share 
#>  0.7421423  0.9657596  0.7723415 
```
