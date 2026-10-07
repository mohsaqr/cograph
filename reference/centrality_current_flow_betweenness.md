# Current-Flow Betweenness

Current-flow betweenness (Brandes and Fleischer 2005), also known as
random-walk betweenness (Newman 2005), treats the network as an
electrical circuit with the edge weights as conductances. One unit of
current is sent between every pair \\s, t\\, and the score is the
average current that flows through the node: \$\$CFB(v) =
\frac{2}{(n-1)(n-2)} \sum\_{s \< t} \frac{1}{2} \sum\_{u} w\_{vu}
\left\| p^{(st)}\_v - p^{(st)}\_u \right\|,\$\$ where \\p^{(st)}\\ are
the node potentials of that pair, taken from the pseudoinverse of the
Laplacian, and the inner sum is set to zero for \\v = s\\ and \\v = t\\.

## Usage

``` r
centrality_current_flow_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is defined for connected undirected networks. On a
disconnected network every score is `NA`, with an unclassed warning. On
a directed network the Laplacian is built from the asymmetric weight
matrix, and `directed = FALSE` gives the undirected reading. With
`weighted = FALSE` the potentials still come from the weighted Laplacian
while the currents are read off the binary adjacency matrix. The fixed
factor \\2/((n-1)(n-2))\\ is the normalization of
`networkx::current_flow_betweenness_centrality()`.

## References

Brandes, U., & Fleischer, D. (2005). Centrality measures based on
current flow. In STACS 2005, Lecture Notes in Computer Science, 3404
(pp. 533-544). Springer.
[doi:10.1007/978-3-540-31856-9_44](https://doi.org/10.1007/978-3-540-31856-9_44)
.

Newman, M. E. J. (2005). A measure of betweenness centrality based on
random walks. Social Networks, 27(1), 39-54.
[doi:10.1016/j.socnet.2004.11.009](https://doi.org/10.1016/j.socnet.2004.11.009)
.

## See also

[`centrality_current_flow_closeness`](https://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_current_flow_betweenness(regulation_net, directed = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.15616791 0.18869976 0.17850158 0.21581651 0.18998872 0.18220790 0.08378783 
#>   Evaluate     Create      Share 
#> 0.16452129 0.13879743 0.17551073 
```
