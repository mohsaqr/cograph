# Flow Betweenness

Flow betweenness (Freeman, Borgatti and White 1991) sums, over pairs of
other nodes \\s\\ and \\t\\, the flow that passes through the node when
a maximum flow is sent from \\s\\ to \\t\\ with the edge weights as
capacities.

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
`cograph_needs_igraph` without it. The flow through a node is read from
the maximum flow that
[`igraph::max_flow()`](https://r.igraph.org/reference/max_flow.html)
returns. On a directed network the sum runs over ordered pairs along the
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
#>       9.52       6.33      13.60      16.27       6.66       8.19       4.67 
#>   Evaluate     Create      Share 
#>       7.26      12.18      12.88 
```
