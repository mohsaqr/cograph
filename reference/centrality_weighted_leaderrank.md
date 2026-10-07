# Weighted LeaderRank

Weighted LeaderRank (Li et al. 2014) adds a ground node \\g\\ that is
linked in both directions to every node. Each original arc and each arc
into the ground has weight 1, and the arc from the ground to node \\i\\
has weight \\(k_i^{in})^{\alpha}\\, where \\k_i^{in}\\ is the in-degree
before the ground is added. The score is the stationary resource of the
random walk on the row-normalized weights, started with one unit on
every node and the ground included.

## Usage

``` r
centrality_weighted_leaderrank(x, wlr_alpha = 1, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- wlr_alpha:

  Exponent \\\alpha\\ of the in-degree, a finite number. Default 1, one
  of the values studied by Li et al. (2014).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Arcs keep their direction, and an undirected edge counts as two opposite
arcs. Edge weights, loops and parallel arcs are ignored, and `mode` has
no effect. The returned scores omit the ground, so they sum to less than
\\N+1\\. With \\\alpha = 0\\ every ground arc has weight 1. With a
positive \\\alpha\\ a node with in-degree 0 scores 0, and when every
in-degree is 0 all scores are `NaN` without a warning. A negative
\\\alpha\\ requires a positive in-degree at every node and raises an
error otherwise.

## References

Li, Q., Zhou, T., Lu, L., & Chen, D. (2014). Identifying influential
spreaders by weighted LeaderRank. Physica A, 404, 47-55.
[doi:10.1016/j.physa.2014.02.041](https://doi.org/10.1016/j.physa.2014.02.041)
.

## See also

[`centrality_leaderrank`](https://sonsoles.me/cograph/reference/centrality_leaderrank.md),
[`centrality_adaptive_leaderrank`](https://sonsoles.me/cograph/reference/centrality_adaptive_leaderrank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_weighted_leaderrank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.2645536  0.4800665  1.5318985  1.0899050  1.0633537  0.5116224  0.3520438 
#>   Evaluate     Create      Share 
#>  0.4305966  0.9572521  0.9316820 
```
