# VoteRank Centrality

VoteRank (Zhang et al. 2016) elects spreaders one at a time. Every node
votes for its neighbors with a voting ability that starts at 1, the node
with the most votes is elected, and each neighbor of the elected node
loses \\1/\langle k \rangle\\ of its ability, where \\\langle k
\rangle\\ is the mean degree. A node elected in round \\r\\ of \\m\\
rounds scores \\(m + 1 - r)/m\\, so the first node elected scores 1.

## Usage

``` r
centrality_voterank(x, ...)
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

Edge weights are ignored. On a directed network a node receives the
votes of its in-neighbors, the election reduces the ability of the
out-neighbors of the elected node, and the mean degree counts in-ties
and out-ties. Every node is elected in turn, so on \\n\\ nodes the
scores are \\1/n, 2/n, \ldots, 1\\. A tie in votes goes to the node
listed first.

## References

Zhang, J.-X., Chen, D.-B., Dong, Q., & Zhao, Z.-D. (2016). Identifying a
set of influential spreaders in complex networks. Scientific Reports, 6,
27823. [doi:10.1038/srep27823](https://doi.org/10.1038/srep27823) .

## See also

[`centrality_ncvoterank`](https://sonsoles.me/cograph/reference/centrality_ncvoterank.md),
[`centrality_wvoterank`](https://sonsoles.me/cograph/reference/centrality_wvoterank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_voterank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.8        0.5        1.0        0.4        0.9        0.6        0.3 
#>   Evaluate     Create      Share 
#>        0.2        0.7        0.1 
```
