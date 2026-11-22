# Flow Betweenness

Flow betweenness (Freeman, Borgatti and White 1991) sums, over pairs of
other nodes \\s\\ and \\t\\, the part of the maximum flow from \\s\\ to
\\t\\ that must pass through the node, with the edge weights as
capacities: \$\$FB(v) = \sum\_{s \neq t \neq v} \left\[ f\_{st}(G) -
f\_{st}(G - v) \right\],\$\$ where \\f\_{st}(G - v)\\ is the maximum
flow once \\v\\ is removed.

## Usage

``` r
centrality_flow_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`), `invert_weights`
  (default `NULL`, which is `TRUE` for tna input), `alpha` (inversion
  exponent, default 1) and `normalized` (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure requires the igraph package and raises an error of class
`cograph_needs_igraph` without it. Only maximum-flow values enter
([`igraph::max_flow()`](https://r.igraph.org/reference/max_flow.html)),
which are unique, so the result does not depend on the solver. The
values equal
[`sna::flowbet()`](https://rdrr.io/pkg/sna/man/flowbet.html) with its
default `cmode = "rawflow"`. Up to cograph 2.7.18 the flow through a
node was read from the one maximum flow igraph returned, which is not
unique. On a directed network the sum runs over ordered pairs along the
edge direction, and on an undirected network over unordered pairs.
`weighted = FALSE` gives every edge capacity one, and
`invert_weights = TRUE` uses \\1/w^\alpha\\ as the capacity.

## References

Freeman, L. C., Borgatti, S. P., & White, D. R. (1991). Centrality in
valued graphs: A measure of betweenness based on network flow. Social
Networks, 13(2), 141-154.
[doi:10.1016/0378-8733(91)90017-N](https://doi.org/10.1016/0378-8733%2891%2990017-N)
.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_flow_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       6.51       5.32       9.18      10.47       4.50       4.85       3.38 
#>   Evaluate     Create      Share 
#>       3.04       7.39       8.71 
```
