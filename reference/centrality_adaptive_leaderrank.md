# Adaptive LeaderRank

Adaptive LeaderRank (Xu and Wang 2017) adds a ground node with H-index 1
that is linked in both directions to every node, and weights each arc
from \\j\\ to \\i\\ by the H-index \\h_i\\ of its target. The H-index of
a node is the largest \\h\\ such that at least \\h\\ of its neighbors
have degree at least \\h\\. The score is the stationary resource of the
random walk on the row-normalized weights, started with one unit on
every node and none on the ground.

## Usage

``` r
centrality_adaptive_leaderrank(x, alr_h_mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- alr_h_mode:

  Neighbors and degrees used for the H-index: `"all"` (default), `"out"`
  or `"in"`. On an undirected network the three agree.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The random walk keeps the direction of the arcs, and an undirected edge
counts as two opposite arcs. Edge weights, loops and parallel arcs are
ignored, and `mode` has no effect. H-indices are computed once on the
original network. With `alr_h_mode = "all"` they use the simple
undirected skeleton, `"out"` uses the out-degrees of out-neighbors and
`"in"` the in-degrees of in-neighbors. The returned scores omit the
ground, so they sum to less than \\N\\. A node with H-index 0 scores 0,
and when every H-index is 0 all scores are `NaN` without a warning.

## References

Xu, S., & Wang, P. (2017). Identifying important nodes by adaptive
LeaderRank. Physica A, 469, 654-664.
[doi:10.1016/j.physa.2016.11.034](https://doi.org/10.1016/j.physa.2016.11.034)
.

## See also

[`centrality_weighted_leaderrank`](https://sonsoles.me/cograph/reference/centrality_weighted_leaderrank.md),
[`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_adaptive_leaderrank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.3925921  0.5545210  1.5092781  1.2537922  0.9927462  0.5961666  0.3916223 
#>   Evaluate     Create      Share 
#>  0.4313295  1.0629935  1.1276256 
```
