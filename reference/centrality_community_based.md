# Community-Based Centrality, Comm Centrality and Community-Based Mediator

Three community-aware measures that need a partition (`membership`).

## Usage

``` r
centrality_community_based(x, membership = NULL, mode = "all", ...)

centrality_comm_centrality(
  x,
  membership = NULL,
  mode = "all",
  comm_r = "max_intra",
  ...
)

centrality_community_mediator(x, membership = NULL, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- membership:

  Community labels, one per node. Required; without it the function
  warns and returns `NA`.

- mode:

  For directed networks: `"all"` (default), `"out"`, or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- comm_r:

  Scale \\R\\ of Comm centrality: `"max_intra"` (default) or a positive
  number.

## Value

Named numeric vector, one value per node.

## Details

- `community_based` (Zhao, Wang, Zhang & Zhu 2015):

  \\CbC(i) = \sum_w d\_{iw} S_w / N\\: every link of \\i\\ counts the
  size \\S_w\\ of the community it lands in. No parameters. Reproduces
  Table 1 of the paper and Table 1 of Tulu et al. (2018).

- `comm_centrality` (Gupta, Singh & Cherifi 2016):

  \$\$CC(i) = (1 + \mu_C)\\ \frac{k^{in}\_i}{\max\_{j \in C} k^{in}\_j}
  R + (1 - \mu_C) \left(\frac{k^{out}\_i}{\max\_{j \in C} k^{out}\_j}
  R\right)^2,\$\$ where \\k^{in}, k^{out}\\ are the intra- and
  inter-community degrees, \\\mu_C\\ the mean inter-link fraction in
  \\i\\'s community, and \\R\\ a scale. The default
  `comm_r = "max_intra"` is the paper's recommended \\R = \max\_{j \in
  C} k^{in}\_j\\ per community; a number applies one global \\R\\. The
  equation uses \\1 + \mu_C\\ although the paper's prose says \\\mu_C\\;
  the equation is implemented. A community without intra (inter) links
  contributes 0 through that term.

- `community_mediator` (Tulu, Hou & Younas 2018):

  \\CbM(i) = H_i \\ d_i / \sum_j d_j\\, with \\H_i\\ the base-2 Shannon
  entropy of \\i\\'s link distribution over the communities. Nodes
  linked to one community only score 0. Base 2 is what reproduces the
  paper's Table 1.

Higher = more central in all three. Under `mode = "out"` or `"in"` only
out- or in-links count; edge weights are ignored.

## Conditions

Raises an error of class `cograph_bad_membership` when `membership` is
not one non-missing label per node.

## References

Zhao, Z., Wang, X., Zhang, W., & Zhu, Z. (2015). A community-based
approach to identifying influential spreaders. Entropy, 17(4),
2228-2252.

Gupta, N., Singh, A., & Cherifi, H. (2016). Centrality measures for
networks with community structure. Physica A, 452, 46-59.

Tulu, M. M., Hou, R., & Younas, T. (2018). Identifying influential nodes
based on community structure to speed up the dissemination of
information in complex network. IEEE Access, 6, 7390-7401.

## See also

[`centrality_community_hub_bridge`](https://sonsoles.me/cograph/reference/centrality_community_hub_bridge.md),
[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_community_based(adj, membership = c(1, 1, 1, 2, 2, 2))
#>   A   B   C   D   E   F 
#> 1.0 1.0 1.5 1.5 1.0 1.0 
centrality_comm_centrality(adj, membership = c(1, 1, 1, 2, 2, 2))
#>        A        B        C        D        E        F 
#> 2.222222 2.222222 5.777778 5.777778 2.222222 2.222222 
centrality_community_mediator(adj, membership = c(1, 1, 1, 2, 2, 2))
#>         A         B         C         D         E         F 
#> 0.0000000 0.0000000 0.1967777 0.1967777 0.0000000 0.0000000 
```
