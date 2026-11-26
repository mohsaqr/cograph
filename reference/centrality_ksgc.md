# K-Shell Based Gravity Centrality

K-shell based gravity centrality (KSGC; Yang and Xiao 2021) is a gravity
model in which the pull of a node on another depends on their k-shell
indices: \$\$KSGC(i) = \sum\_{j:\\d\_{ij} \le \langle d \rangle / 2}
\exp\\\Bigl(\frac{ks_i - ks_j}{ks\_{max} - ks\_{min}}\Bigr) \frac{k_i
k_j}{d\_{ij}^2},\$\$ with \\k\\ the degree, \\ks\\ the k-shell index,
\\d\_{ij}\\ the distance and \\\langle d \rangle\\ the average
shortest-path length of the network.

## Usage

``` r
centrality_ksgc(x, ...)
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
The radius is half the average path length, used as it is in the test
\\d\_{ij} \le \langle d \rangle / 2\\; it is not rounded, unlike the
`"auto"` radius of
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md).
On a network whose average path length is below 2 no node lies within
the radius, and every score is 0. This is the rule of the source, and it
makes the measure uninformative on small dense networks. When all nodes
are in one shell the exponent is 0/0 and the coefficient is taken as 1,
which is the plain gravity model; the source says the method reduces to
it when two nodes share a shell. The source covers connected networks;
on a disconnected one the average is taken over the pairs that reach
each other. Isolated nodes score 0. The source prints no value on a
small network, so the implementation is checked against the definition
only. `normalized = TRUE` divides the scores by their maximum.

## References

Yang, X. and Xiao, F. (2021). An improved gravity model to identify
influential nodes in complex networks based on k-shell method.
Knowledge-Based Systems, 227, 107198.
[doi:10.1016/j.knosys.2021.107198](https://doi.org/10.1016/j.knosys.2021.107198)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ksgc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          0          0          0          0          0          0          0 
#>   Evaluate     Create      Share 
#>          0          0          0 
```
