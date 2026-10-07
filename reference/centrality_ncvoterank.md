# NCVoteRank

NCVoteRank (Kumar and Panda 2020) is VoteRank (Zhang et al. 2016) with
the voting ability of each voter weighted by its neighborhood coreness.
A node collects the score \$\$s_u = \sum\_{v \in N(u)} va_v \\\[\theta +
(1 - \theta)\\ nc_v\], \qquad nc_v = \frac{\sum\_{w \in N(v)}
ks(w)}{\max_j \sum\_{w \in N(j)} ks(w)},\$\$ with \\ks\\ the k-shell
index. The top scorer is elected, its ability drops to 0, its neighbors
lose \\1/\langle k \rangle\\ and the nodes two steps away lose
\\1/(2\langle k \rangle)\\.

## Usage

``` r
centrality_ncvoterank(x, ncvote_theta = 0.5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ncvote_theta:

  Weight \\\theta\\ of the plain vote (default 0.5).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights and loops are ignored. Elections continue
until every node is placed. The first elected scores 1 and the last
\\1/n\\, so scores lie in \\(0, 1\]\\. The definition follows the
Centrality Zoo and three independent restatements of the article, and
the scaling of the coreness by its maximum follows Yu et al. (2020).
With \\\theta = 1\\ and no two-step weakening the procedure is VoteRank.

## References

Kumar, S., & Panda, B. S. (2020). Identifying influential nodes in
social networks: Neighborhood coreness based voting approach. Physica A,
553, 124215.

Zhang, J.-X., Chen, D.-B., Dong, Q., & Zhao, Z.-D. (2016). Identifying a
set of influential spreaders in complex networks. Scientific Reports, 6,
27823.

## See also

[`centrality_voterank`](https://sonsoles.me/cograph/reference/centrality_voterank.md),
[`centrality_wvoterank`](https://sonsoles.me/cograph/reference/centrality_wvoterank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ncvoterank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.4        0.6        1.0        0.7        0.5        0.9        0.3 
#>   Evaluate     Create      Share 
#>        0.2        0.8        0.1 
```
