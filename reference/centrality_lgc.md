# Laplacian Gravity Centrality

Laplacian gravity centrality (LGC; Zhang et al. 2022) is a gravity model
whose masses are the Laplacian centralities of the nodes: \$\$LGC(i) =
\sum\_{j \ne i:\\d\_{ij} \le \langle d \rangle / 2}
\frac{LC(i)\\LC(j)}{d\_{ij}^2}, \qquad LC(i) = k_i^2 + k_i + 2 \sum\_{j
\in N(i)} k_j,\$\$ with \\k\\ the degree, \\d\_{ij}\\ the distance and
\\\langle d \rangle\\ the average shortest-path length of the network.

## Usage

``` r
centrality_lgc(x, ...)
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
\\LC\\ is the drop in Laplacian energy when the node is removed, the
score of
[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md)
on an unweighted network; the Centrality Zoo uses the relative drop,
which changes every score by one constant. The radius is half the
average path length, used as it is in the test; the source says that its
radius of 1.73 admits neighbors only. On a network whose average path
length is below 2 no node lies within the radius, and every score is 0.
This is the rule of the source, and it makes the measure uninformative
on small dense networks. Pairs in different components contribute
nothing, as the source states, and the average is then taken over the
pairs that reach each other. The implementation reproduces the 21
Laplacian centralities of the source's Table 1, the score of its worked
example (4788) and the 21 rescaled scores of its Table 2, for the ARPA
network on an edge list read from its figure. Isolated nodes score 0.
`normalized = TRUE` divides the scores by their maximum.

## References

Zhang, Q., Shuai, B. and Lü, M. (2022). A novel method to identify
influential nodes in complex networks based on gravity centrality.
Information Sciences, 618, 98-117.
[doi:10.1016/j.ins.2022.10.070](https://doi.org/10.1016/j.ins.2022.10.070)
.

## See also

[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md),
[`centrality_ksgc`](https://sonsoles.me/cograph/reference/centrality_ksgc.md),
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lgc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          0          0          0          0          0          0          0 
#>   Evaluate     Create      Share 
#>          0          0          0 
```
