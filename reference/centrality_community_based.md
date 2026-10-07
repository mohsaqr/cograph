# Community-Based Centralities

Three measures that score a node from a partition `membership`.
Community-based centrality (Zhao et al. 2015) is \\CbC(i) = \sum_w
d\_{iw} S_w / N\\, where \\d\_{iw}\\ counts the links of \\i\\ into
community \\w\\ of size \\S_w\\. Comm centrality (Gupta, Singh and
Cherifi 2016) combines intra-community degree \\k^{in}\\ and
inter-community degree \\k^{out}\\: \$\$CC(i) = (1 + \mu_C)
\frac{k^{in}\_i}{\max\_{j \in C} k^{in}\_j} R + (1 - \mu_C)
\left(\frac{k^{out}\_i}{\max\_{j \in C} k^{out}\_j} R\right)^2,\$\$ with
\\\mu_C\\ the mean inter-link fraction in the community of \\i\\.
Community-based mediator centrality (Tulu, Hou and Younas 2018) is
\\CbM(i) = H_i\\ d_i / \sum_j d_j\\, with \\H_i\\ the base-2 entropy of
the links of \\i\\ over the communities.

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

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- membership:

  Community labels, one per node, for example from
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

- comm_r:

  Scale \\R\\ of Comm centrality: `"max_intra"` (default) or a single
  positive number applied to every community.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights and self-loops are ignored. Under `mode = "out"` or
`mode = "in"` only out-links or in-links count, and the default ignores
direction. Higher values mark more central nodes in all three. The
default `comm_r = "max_intra"` sets \\R\\ to the largest intra-community
degree of each community, the choice the source recommends. The prose of
Gupta et al. writes \\\mu_C\\ where their equation has \\1 + \mu_C\\,
and the equation is implemented. Nodes linked to one community only
score 0 on the mediator measure. Without `membership` each function
raises a warning of classes `cograph_bad_membership` and
`cograph_undefined_measure` and returns `NA`. A `membership` that is not
one non-missing label per node raises an error of class
`cograph_bad_membership`, and an invalid `comm_r` raises
`cograph_bad_parameter`.

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
[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_community_based(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        2.5        3.0        3.5        3.0        2.5        2.5        2.0 
#>   Evaluate     Create      Share 
#>        2.5        3.0        2.5 
centrality_comm_centrality(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   4.428686   4.765714   6.954171   5.299886   4.428686   3.760000   1.980000 
#>   Evaluate     Create      Share 
#>   3.760000   6.453750   3.760000 
centrality_community_mediator(regulation_net,
                              membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.08990283 0.07222471 0.12771476 0.10203287 0.08990283 0.06684519 0.00000000 
#>   Evaluate     Create      Share 
#> 0.06684519 0.11111111 0.06684519 
```
