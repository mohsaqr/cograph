# LineRank Centrality

LineRank (Kang et al. 2011) computes PageRank on the line graph, whose
nodes are the edges of the network, and gives each node the sum of the
stationary probabilities of its incident edges. In a directed network
edge \\e\\ leads to edge \\f\\ when the target of \\e\\ is the source of
\\f\\. In an undirected network two edges are adjacent when they share
an endpoint (Kosa et al. 2015).

## Usage

``` r
centrality_linerank(
  x,
  damping = 0.85,
  linerank_aggregation = "probability",
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- damping:

  Probability of continuing the walk on the line graph, in \\\[0, 1)\\.
  Default 0.85.

- linerank_aggregation:

  `"probability"` (default) sums the stationary edge probabilities.
  `"weight"` multiplies them by the edge weights first.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`) and
  `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The walk on the line graph moves to an adjacent edge with probability
proportional to that edge's weight and jumps to a uniformly chosen edge
with probability `1 - damping`. An edge without successors also jumps
uniformly. With `linerank_aggregation = "probability"` the scores sum to
two on a network with edges. With `"weight"` each stationary probability
is multiplied by its edge weight before aggregation, following the
weighted incidence matrix of Algorithm 2 in Kang et al. (2011). A loop
contributes twice to its node, and an isolated node scores zero. Edge
weights must be finite and nonnegative, and `weighted = FALSE` gives
every edge weight one. A `damping` outside \\\[0, 1)\\ raises an error.

## References

Kang, U., Papadimitriou, S., Sun, J., & Tong, H. (2011). Centralities in
Large Networks: Algorithms and Observations. Proceedings of the 2011
SIAM International Conference on Data Mining, 119-130.
[doi:10.1137/1.9781611972818.11](https://doi.org/10.1137/1.9781611972818.11)
.

Kosa, B., Balassi, M., Englert, P., & Kiss, A. (2015). Betweenness
versus Linerank. Computer Science and Information Systems, 12(1), 33-48.
[doi:10.2298/CSIS141101092K](https://doi.org/10.2298/CSIS141101092K) .

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_linerank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.23711430 0.08083638 0.37402059 0.24709747 0.23941541 0.13042197 0.06670327 
#>   Evaluate     Create      Share 
#> 0.14344583 0.28715250 0.19379228 
```
