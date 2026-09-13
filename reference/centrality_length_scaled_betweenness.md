# Betweenness and closeness variants that carry a tuning parameter

Four measures that reweight, rescope or re-tune a measure
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
already computes. Each is a thin wrapper on
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md).

## Usage

``` r
centrality_length_scaled_betweenness(x, ...)

centrality_delta_betweenness(x, betweenness_delta = 1, ...)

centrality_ego_betweenness(x, ...)

centrality_delta_closeness(x, mode = "all", closeness_delta = 1, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- betweenness_delta:

  Decay exponent for `centrality_delta_betweenness`. Default 1.

- mode:

  Direction: `"all"`, `"out"` or `"in"`.

- closeness_delta:

  Distance exponent for `centrality_delta_closeness`. Default 1.

## Value

Named numeric vector, one value per node.

## Details

- `length_scaled_betweenness` (Borgatti & Everett 2006; Brandes 2008,
  Algorithm 5):

  Betweenness with each separated pair weighted by \\1 / d(s,t)\\, so
  brokering between nearby nodes counts for more than brokering across
  the graph.

- `delta_betweenness` (Agneessens, Borgatti & Everett 2017):

  Betweenness with the pair weight \\(d(s,t) - 1)^{-\delta}\\
  (`betweenness_delta`, default 1). At \\\delta = 0\\ it is ordinary
  betweenness; raising it concentrates the score on locally brokered
  pairs.

- `ego_betweenness` (Everett & Borgatti 2005):

  Betweenness computed inside the node's own ego network rather than the
  whole graph. A node with fewer than two neighbors scores 0. It is
  close to, but not a function of, `effective_size`.

- `delta_closeness` (Agneessens, Borgatti & Everett 2017, eq. 2):

  \\\sum_j d\_{ij}^{-\delta} / (n-1)\\ (`closeness_delta`, default 1).
  One exponent spans the closeness family: \\\delta = 1\\ is `harmonic`
  over \\n-1\\, \\\delta = 2\\ is `harary` over \\n-1\\, a large
  \\\delta\\ approaches degree, and \\\delta = 0\\ counts the reachable
  set.

Bounded-distance betweenness, which the Centrality Zoo lists as
"k-betweenness", needs no separate measure: it is
`centrality(x, measures = "betweenness", cutoff = k)`.

## References

Agneessens, F., Borgatti, S. P., & Everett, M. G. (2017). Geodesic based
centrality: Unifying the local and the global. Social Networks, 49,
12-26.

Brandes, U. (2008). On variants of shortest-path betweenness centrality
and their generic computation. Social Networks, 30(2), 136-145.

Everett, M., & Borgatti, S. P. (2005). Ego network betweenness. Social
Networks, 27(1), 31-38.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_length_scaled_betweenness(adj)
#>        A        B        C        D        E        F 
#> 0.000000 0.000000 2.333333 2.333333 0.000000 0.000000 
centrality_delta_betweenness(adj, betweenness_delta = 2)
#> A B C D E F 
#> 0 0 3 3 0 0 
centrality_ego_betweenness(adj)
#> A B C D E F 
#> 0 0 2 2 0 0 
centrality_delta_closeness(adj, closeness_delta = 2)
#>         A         B         C         D         E         F 
#> 0.4944444 0.4944444 0.7000000 0.7000000 0.4944444 0.4944444 
```
