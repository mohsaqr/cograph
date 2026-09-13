# Community Hub-Bridge Centrality

Ghalmane, El Hassouni and Cherifi's (2019) score for nodes that are both
hubs inside their community and bridges between communities: \$\$CHB(i)
= \|C_i\| \\ k^{intra}\_i + NNC_i \\ k^{inter}\_i,\$\$ where \\\|C_i\|\\
is the number of nodes in \\i\\'s own community, \\k^{intra}\_i\\ and
\\k^{inter}\_i\\ its numbers of links inside and outside that community,
and \\NNC_i\\ the number of *other* communities it is linked to (eqs. 2
to 4 of the paper). Higher values mark nodes whose removal both
fragments their community and cuts links between communities. A
normalised variant with the same name exists in later work by the same
group; this is the original raw form.

## Usage

``` r
centrality_community_hub_bridge(x, membership = NULL, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- membership:

  Community labels, one per node. Required; without it the function
  warns and returns `NA`. Obtain one from
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

- mode:

  For directed networks: `"all"` (default), `"out"`, or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node.

## Details

Under `mode = "out"` or `"in"` only out- or in-links count; the default
ignores direction. Edge weights are ignored.

## Conditions

Raises an error of class `cograph_bad_membership` when `membership` is
not one non-missing label per node.

## References

Ghalmane, Z., El Hassouni, M., & Cherifi, H. (2019). Immunization of
networks with non-overlapping community structure. Social Network
Analysis and Mining, 9, 45.

## See also

[`centrality_modularity_vitality`](https://sonsoles.me/cograph/reference/centrality_modularity_vitality.md),
[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_community_hub_bridge(adj, membership = c(1, 1, 1, 2, 2, 2))
#> A B C D E F 
#> 6 6 7 7 6 6 
```
