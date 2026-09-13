# NCVoteRank

Kumar and Panda's (2020) neighborhood-coreness VoteRank. As in VoteRank,
every node votes for its neighbors with its voting ability, the top
scorer is elected, and the abilities around it are weakened; here each
voter's ability is additionally weighted by its neighborhood coreness,
\$\$s_u = \sum\_{v \in N(u)} va_v \\\[\theta + (1 - \theta)\\ nc_v\],
\qquad nc_v = \frac{\sum\_{w \in N(v)} ks(w)} {\max_j \sum\_{w \in N(j)}
ks(w)},\$\$ with \\ks\\ the k-shell index (Bae & Kim 2014) and \\\theta
= 0.5\\. After an election the winner's ability drops to 0, its
neighbors lose \\1 / \langle k \rangle\\ and the nodes two steps away
lose \\1 / (2 \langle k \rangle)\\. Elections continue until every node
is placed, as in
[`centrality_voterank`](https://sonsoles.me/cograph/reference/centrality_voterank.md);
the first elected scores 1, the last \\1 / n\\.

## Usage

``` r
centrality_ncvoterank(x, ncvote_theta = 0.5, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ncvote_theta:

  Weight \\\theta\\ of the plain vote. Default 0.5.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in \\(0, 1\]\\, one score per node.

## Details

**Provenance.** The original Physica A article could not be obtained;
this definition follows the Centrality Zoo encyclopedia (Shvydun 2025)
and three independent restatements (Yu et al. 2020, Li et al. 2022, Zhu
et al. 2023), which agree on the voter-side coreness weighting. The
scaling of the coreness term by its maximum follows Yu et al., who state
the coreness is normalized without giving the form. With \\\theta = 1\\
and no two-hop weakening the procedure is exactly VoteRank, which is
reproduced against `networkx.voterank`. Defined for undirected graphs;
direction, weights and loops are ignored.

## References

Kumar, S., & Panda, B. S. (2020). Identifying influential nodes in
social networks: Neighborhood coreness based voting approach. Physica A,
553, 124215.

Zhang, J.-X., Chen, D.-B., Dong, Q., & Zhao, Z.-D. (2016). Identifying a
set of influential spreaders in complex networks. Scientific Reports, 6,
27823.

## See also

[`centrality_voterank`](https://sonsoles.me/cograph/reference/centrality_voterank.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_ncvoterank(adj)
#>         A         B         C         D         E         F 
#> 0.6666667 0.3333333 1.0000000 0.8333333 0.5000000 0.1666667 
```
