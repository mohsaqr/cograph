# LeaderRank Centrality

LeaderRank (Lu et al. 2011) adds a ground node joined in both directions
to every node and runs a random walk without damping on the extended
network. The final score of the ground node is shared equally among the
other nodes.

## Usage

``` r
centrality_leaderrank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure needs a directed network. On undirected input every score is
`NA` with a `cograph_undefined_measure` warning. Edge weights are
ignored;
[`centrality_weighted_leaderrank`](https://sonsoles.me/cograph/reference/centrality_weighted_leaderrank.md)
uses them. The walk starts with one unit at every node and none at the
ground node, so the scores sum to \\n\\. The values equal
[`centiserve::leaderrank()`](https://rdrr.io/pkg/centiserve/man/leaderrank.html).

## References

Lu, L., Zhang, Y.-C., Yeung, C. H., & Zhou, T. (2011). Leaders in social
networks, the Delicious case. PLoS ONE, 6(6), e21202.
[doi:10.1371/journal.pone.0021202](https://doi.org/10.1371/journal.pone.0021202)
.

## See also

[`centrality_weighted_leaderrank`](https://sonsoles.me/cograph/reference/centrality_weighted_leaderrank.md),
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_leaderrank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.2831245  0.7493777  1.4385276  1.1627388  1.1517175  0.7553269  0.6661930 
#>   Evaluate     Create      Share 
#>  0.6876645  1.0614717  1.0438580 
```
