# WVoteRank, EnRenew and VoteRank++

Three further spreader-selection procedures in the VoteRank family. All
three elect one node per round until every node is placed and return the
election order as a score, 1 for the first elected down to \\1 / n\\;
ties go to the lowest node index. Direction and self-loops are ignored.

## Usage

``` r
centrality_wvoterank(x, ...)

centrality_enrenew(x, enrenew_depth = 2, ...)

centrality_voterank_plus(x, voterank_lambda = 0.1, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- enrenew_depth:

  Renewal radius \\l\\ for `enrenew`. Default 2.

- voterank_lambda:

  Suppression factor \\\lambda\\ for `voterank_plus`. Default 0.1.

## Value

Named numeric vector in \\(0, 1\]\\, one score per node.

## Details

- `wvoterank` (Sun, Chen, He & Ch'ng 2019):

  VoteRank for weighted graphs: \\s_v = \sqrt{k_v \sum\_{u \in N(v)}
  va_u w\_{vu}}\\. After an election the winner's ability is 0 and its
  neighbors lose \\1 / \langle w \rangle\\, where \\\langle w \rangle\\
  is the average strength (the paper's Figure 1 pins strength, not
  degree). Uses edge weights; with unit weights it is VoteRank with a
  square-root score. Reproduces all sixty numbers of the paper's Figure
  1.

- `enrenew` (Guo, Yang, Guo, Pan & Chen 2020):

  Entropy-based selection: \\E_v = \sum\_{u \in N(v)} -p\_{uv} \ln
  p\_{uv}\\ with \\p\_{uv} = k_u / \sum\_{l \in N(v)} k_l\\; after
  electing the largest \\E\\, every entropy term flowing outward to
  depth \\d \le l\\ is scaled by \\1 - 1 / (2^{d-1} \ln \langle k
  \rangle)\\, with \\l\\ = `enrenew_depth` (default 2). Reproduces the
  paper's Figure 1. The authors' released code differs from the paper in
  several ways; the paper is implemented. Note the factor turns negative
  when \\\langle k \rangle \< e\\.

- `voterank_plus` (Liu, Li, Fang & Yao 2021):

  Initial ability \\\ln(1 + k_i / k\_{\max})\\, degree-proportional vote
  shares over unelected neighbors, score \\\sqrt{k_i \sum_j va_j w\_{j
  \to i}}\\, and after an election abilities are multiplied by
  \\\lambda\\ one step away and \\\sqrt{\lambda}\\ two steps away
  (`voterank_lambda`, default 0.1). The article is closed access; the
  implementation matches the authors' released code exactly, including
  its exclusion of elected nodes from the vote-share denominator.

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
[`centrality_ncvoterank`](https://sonsoles.me/cograph/reference/centrality_ncvoterank.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_wvoterank(adj)
#>         A         B         C         D         E         F 
#> 0.6666667 0.3333333 1.0000000 0.8333333 0.5000000 0.1666667 
centrality_enrenew(adj)
#>         A         B         C         D         E         F 
#> 0.6666667 0.3333333 1.0000000 0.8333333 0.5000000 0.1666667 
centrality_voterank_plus(adj)
#>         A         B         C         D         E         F 
#> 0.6666667 0.3333333 1.0000000 0.8333333 0.5000000 0.1666667 
```
