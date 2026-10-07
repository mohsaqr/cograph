# Random Walk Decay Centrality

Random walk decay centrality (Was, Rahwan and Skibski 2019) sums, over
all starting nodes \\u\\, the starting weight \\b_u\\ times the expected
discounted first arrival of a random walk from \\u\\ at node \\v\\. With
\\T_v\\ the first arrival time and \\a\\ the decay factor `rwd_decay`,
\$\$RWD_v = \sum_u b_u \\ E_u\left\[a^{T_v}; T_v \< \infty\right\].\$\$

## Usage

``` r
centrality_random_walk_decay(x, rwd_decay = 0.5, rwd_node_weights = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- rwd_decay:

  Decay factor \\a\\, a finite number in \\\[0, 1)\\. Default 0.5.

- rwd_node_weights:

  Nonnegative starting weights \\b\\, one per node. `NULL` (default)
  gives every node weight one. A named vector is matched to the node
  names.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`) and
  `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The walk follows outgoing edges with probability proportional to their
weights, and an undirected edge is traversed in both directions. A walk
that reaches a node without outgoing edges stops. Loops are kept as
transitions that stay at the node unless `loops = FALSE`. The start
counts as an arrival at time zero, so an isolated node scores its own
starting weight and `rwd_decay = 0` returns the starting weights. Edge
weights must be finite and nonnegative, and `weighted = FALSE` ignores
edge weights while keeping `rwd_node_weights`. Invalid `rwd_decay` or
`rwd_node_weights`, and raw scores that overflow, raise an error.
Example 3 of the paper contains inconsistent numerical values, and the
implementation follows Definition 1, equation 6.

## References

Was, T., Rahwan, T., & Skibski, O. (2019). Random Walk Decay Centrality.
Proceedings of the AAAI Conference on Artificial Intelligence, 33(01),
2197-2204.
[doi:10.1609/aaai.v33i01.33012197](https://doi.org/10.1609/aaai.v33i01.33012197)
.

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality_random_walk`](https://sonsoles.me/cograph/reference/centrality_random_walk.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_random_walk_decay(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.066592   1.278368   2.797831   2.173405   2.359979   1.628358   1.233824 
#>   Evaluate     Create      Share 
#>   1.645843   2.129525   1.834672 
```
