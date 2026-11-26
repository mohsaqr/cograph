# Dynamical Spanning Tree Centrality

Dynamical spanning tree (DST) centrality (Chen et al. 2003, as given by
Lu et al. 2016) is the share of the spanning trees of a network that is
lost when a node is removed: \$\$DST(i) = 1 - \frac{t(G \setminus
\\v_i\\)}{t(G)},\$\$ where \\t\\ counts spanning trees.

## Usage

``` r
centrality_dst(x, ...)
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

A named numeric vector with one score per node, in input node order,
between 0 and 1, or `NA` at every node when the network is disconnected.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The counts come from the matrix-tree theorem. A node whose removal
disconnects the network scores 1, and a node with one neighbor scores 0.
On a disconnected network and on a single node every score is `NA` with
a `cograph_undefined_measure` warning. The definition follows equation
(50) of Lu et al. (2016); the 2003 paper was not consulted. The measure
is different from the `spanning_tree` measure of
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
`normalized = TRUE` divides the scores by their maximum.

## References

Chen, Y., Hu, A.-Q., Yip, K.-W., Hu, J. and Zhong, Z.-G. (2003). Finding
the most vital node with respect to the number of spanning trees. In
Proceedings of the 2003 International Conference on Neural Networks and
Signal Processing, 2, 1670-1673.
[doi:10.1109/ICNNSP.2003.1281204](https://doi.org/10.1109/ICNNSP.2003.1281204)
.

Lu, L., Chen, D., Ren, X.-L., Zhang, Q.-M., Zhang, Y.-C. and Zhou, T.
(2016). Vital nodes identification in complex networks. Physics Reports,
650, 1-63.
[doi:10.1016/j.physrep.2016.06.007](https://doi.org/10.1016/j.physrep.2016.06.007)
.

## See also

[`centrality_mec`](https://sonsoles.me/cograph/reference/centrality_mec.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dst(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.9105337  0.9425221  0.9592007  0.9455253  0.9185185  0.9086591  0.8545165 
#>   Evaluate     Create      Share 
#>  0.9028054  0.9384110  0.9019912 
```
