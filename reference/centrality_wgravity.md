# Weighted Gravity Model

The weighted gravity model (WGravity; Liu et al. 2020) multiplies the
degree gravity model of a node by the node's entry of the leading
eigenvector of the adjacency matrix: \$\$C\_{WG}(i) = \sum\_{j \ne
i:\\d\_{ij} \le \langle d \rangle / 2}
\frac{e_i\\k_i\\k_j}{d\_{ij}^2},\$\$ with \\e\\ the eigenvector, \\k\\
the degree, \\d\_{ij}\\ the distance and \\\langle d \rangle\\ the
average shortest-path length.

## Usage

``` r
centrality_wgravity(x, ...)
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
The source calls the eigenvector normalized without naming a norm. It
has unit Euclidean length here, which differs from the maximum-one
scaling of
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md)
by one constant per network and changes no ranking. "Weighted" refers to
this weight of the node, not to edge weights. The radius is that of
[`centrality_lgc`](https://sonsoles.me/cograph/reference/centrality_lgc.md):
half the average path length, used as it is, so every score is 0 on a
network whose average path length is below 2. On a disconnected network
the eigenvector is nonzero only on the component with the largest
spectral radius, and the other components score 0. When two components
share that spectral radius the eigenvector is not unique; the measure
then warns (class `cograph_undefined_measure`) and returns `NA`. The
source prints no value of the measure, so the implementation is checked
against the definition only. Isolated nodes score 0. `normalized = TRUE`
divides the scores by their maximum.

## References

Liu, F., Wang, Z. and Deng, Y. (2020). GMM: A generalized mechanics
model for identifying the importance of nodes in complex networks.
Knowledge-Based Systems, 193, 105464.
[doi:10.1016/j.knosys.2019.105464](https://doi.org/10.1016/j.knosys.2019.105464)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_lgc`](https://sonsoles.me/cograph/reference/centrality_lgc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wgravity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          0          0          0          0          0          0          0 
#>   Evaluate     Create      Share 
#>          0          0          0 
```
