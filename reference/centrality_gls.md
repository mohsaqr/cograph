# Global and Local Structure Centrality

Global and local structure centrality (GLS; Sheng et al. 2020)
multiplies a global term, built on the neighbors a node shares with
every other node, by a local term: \$\$GLS(i) = \Bigl(k_i \sum\_{j \ne
i} 1.1^{c\_{ij}}\Bigr) \frac{k_i}{n - 1},\$\$ with \\k_i\\ the degree,
\\c\_{ij}\\ the number of common neighbors of \\i\\ and \\j\\, and \\n\\
the number of nodes.

## Usage

``` r
centrality_gls(x, ...)
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
The formula is the one the numbers of the source follow, which differs
from its printed equations in two places. The global sum runs over all
other nodes, as the text and the worked example of the source say, where
the subscript of its equation (4) names the neighbors only. The local
term is the degree divided by \\n - 1\\: that is what equation (6) gives
in the worked example, which assigns every neighbor the selection
probability of the node itself. This reading reproduces the worked value
(9.58), the rank order of the source's 12-node example and its top ten
nodes of the karate club network; the printed equations reproduce none
of the three. The base 1.1 is fixed by the source. The score ranks nodes
almost like
[`centrality_lnc`](https://sonsoles.me/cograph/reference/centrality_lnc.md)
(Kendall correlation of 0.99 on four test networks). Every other node
adds at least 1 to the sum, including nodes in other components.
Isolated nodes score 0. `normalized = TRUE` divides the scores by their
maximum.

## References

Sheng, J., Dai, J., Wang, B., Duan, G., Long, J., Zhang, J., Guan, K.,
Hu, S., Chen, L. and Guan, W. (2020). Identifying influential nodes in
complex networks based on global and local structure. Physica A:
Statistical Mechanics and its Applications, 541, 123262.
[doi:10.1016/j.physa.2019.123262](https://doi.org/10.1016/j.physa.2019.123262)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_topological_coefficient`](https://sonsoles.me/cograph/reference/centrality_topological_coefficient.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_gls(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   31.65861   47.81524   67.45884   47.53012   31.55197   32.12306   19.87040 
#>   Evaluate     Create      Share 
#>   32.79833   48.16284   32.67000 
```
