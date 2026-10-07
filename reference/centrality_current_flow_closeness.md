# Current-Flow Closeness

Current-flow closeness (Brandes and Fleischer 2005), equal to the
information centrality of Stephenson and Zelen (1989), replaces the
shortest-path distance in closeness by the effective resistance
\\R\_{vw}\\ of the network read as an electrical circuit with the edge
weights as conductances: \$\$CFC(v) = \frac{n - 1}{\sum\_{w \ne v}
R\_{vw}}, \qquad R\_{vw} = L^{+}\_{vv} + L^{+}\_{ww} - 2
L^{+}\_{vw},\$\$ with \\L^{+}\\ the pseudoinverse of the Laplacian.

## Usage

``` r
centrality_current_flow_closeness(x, ...)
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
matrix, and `directed = FALSE` gives the undirected reading. Edge
weights are always used, and `weighted = FALSE` has no effect.

## References

Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
examples. Social Networks, 11(1), 1-37.
[doi:10.1016/0378-8733(89)90016-6](https://doi.org/10.1016/0378-8733%2889%2990016-6)
.

Brandes, U., & Fleischer, D. (2005). Centrality measures based on
current flow. In STACS 2005, Lecture Notes in Computer Science, 3404
(pp. 533-544). Springer.
[doi:10.1007/978-3-540-31856-9_44](https://doi.org/10.1007/978-3-540-31856-9_44)
.

## See also

[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md),
[`centrality_information`](https://sonsoles.me/cograph/reference/centrality_information.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_current_flow_closeness(regulation_net, directed = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6867920  0.7624105  0.7611862  0.7931430  0.6568402  0.7329435  0.4833969 
#>   Evaluate     Create      Share 
#>  0.7485930  0.7090881  0.7652549 
```
