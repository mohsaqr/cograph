# WVoteRank, EnRenew and VoteRank++

Three spreader-selection procedures in the VoteRank family that elect
one node per round. WVoteRank (Sun et al. 2019) scores a node by \\s_v =
\sqrt{k_v \sum\_{u \in N(v)} va_u w\_{vu}}\\ and lowers the ability of
the neighbors of a winner by \\1 / \langle w \rangle\\, the inverse of
the average strength. EnRenew (Guo et al. 2020) elects the largest
neighbor entropy \\E_v = -\sum\_{u \in N(v)} p\_{uv} \ln p\_{uv}\\,
\\p\_{uv} = k_u / \sum\_{l \in N(v)} k_l\\, and scales the entropy terms
within `enrenew_depth` steps by \\1 - 1 / (2^{d-1} \ln \langle k
\rangle)\\. VoteRank++ (Liu et al. 2021) starts from ability \\\ln(1 +
k_i / k\_{\max})\\, splits votes in proportion to degree, and multiplies
abilities by \\\lambda\\ one step from a winner and by
\\\sqrt{\lambda}\\ two steps away.

## Usage

``` r
centrality_wvoterank(x, ...)

centrality_enrenew(x, enrenew_depth = 2, ...)

centrality_voterank_plus(x, voterank_lambda = 0.1, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  WVoteRank uses `weighted` (use edge weights, default `TRUE`).

- enrenew_depth:

  Renewal radius \\l\\ for EnRenew (default 2).

- voterank_lambda:

  Suppression factor \\\lambda\\ for VoteRank++ (default 0.1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Elections continue until every node is placed. The first elected scores
1 and the last \\1/n\\, so scores lie in \\(0, 1\]\\, and ties go to the
lowest node index. Direction and self-loops are ignored. WVoteRank uses
edge weights, and with unit weights it is VoteRank with a square-root
score. The other two ignore weights. The renewal factor of EnRenew is
negative when \\\langle k \rangle \< e\\. EnRenew follows the article
where the released code of the authors differs from it. VoteRank++
follows the released code of the authors, including the exclusion of
elected nodes from the vote-share denominator.

## References

Sun, H.-L., Chen, D.-B., He, J.-L., & Ch'ng, E. (2019). A voting
approach to uncover multiple influential spreaders on weighted networks.
Physica A, 519, 303-312.

Guo, C., Yang, L., Guo, X., Pan, J., & Chen, X. (2020). Influential
nodes identification in complex networks via information entropy.
Entropy, 22(2), 242.

Liu, P., Li, L., Fang, S., & Yao, Y. (2021). Identifying influential
nodes in social networks: A voting approach. Chaos, Solitons & Fractals,
152, 111309.

## See also

[`centrality_voterank`](https://sonsoles.me/cograph/reference/centrality_voterank.md),
[`centrality_ncvoterank`](https://sonsoles.me/cograph/reference/centrality_ncvoterank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wvoterank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.7        0.6        1.0        0.9        0.5        0.8        0.4 
#>   Evaluate     Create      Share 
#>        0.3        0.2        0.1 
centrality_enrenew(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.4        0.8        1.0        0.6        0.2        0.3        0.1 
#>   Evaluate     Create      Share 
#>        0.7        0.9        0.5 
centrality_voterank_plus(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        0.4        0.6        1.0        0.9        0.5        0.7        0.3 
#>   Evaluate     Create      Share 
#>        0.2        0.8        0.1 
```
