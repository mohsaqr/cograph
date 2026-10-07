# Community Hub-Bridge Centrality

Community hub-bridge centrality (Ghalmane, El Hassouni and Cherifi 2019)
scores nodes that are hubs inside their community and bridges between
communities: \$\$CHB(i) = \|C_i\|\\ k^{intra}\_i + NNC_i\\
k^{inter}\_i,\$\$ where \\\|C_i\|\\ is the size of the community of
\\i\\, \\k^{intra}\_i\\ and \\k^{inter}\_i\\ its numbers of links inside
and outside that community, and \\NNC_i\\ the number of other
communities it links to.

## Usage

``` r
centrality_community_hub_bridge(x, membership = NULL, mode = "all", ...)
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

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights and self-loops are ignored. Under `mode = "out"` or
`mode = "in"` only out-links or in-links count, and the default ignores
direction. This is the raw form of the original article. Later work by
the same group uses a normalized variant with the same name. Without
`membership` the function raises an unclassed warning and returns `NA`
for every node. A `membership` that is not one non-missing label per
node raises an error of class `cograph_bad_membership`.

## References

Ghalmane, Z., El Hassouni, M., & Cherifi, H. (2019). Immunization of
networks with non-overlapping community structure. Social Network
Analysis and Mining, 9, 45.

## See also

[`centrality_community_based`](https://sonsoles.me/cograph/reference/centrality_community_based.md),
[`centrality_modularity_vitality`](https://sonsoles.me/cograph/reference/centrality_modularity_vitality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_community_hub_bridge(regulation_net,
                                membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         13         10         19         14         13          9          4 
#>   Evaluate     Create      Share 
#>          9         18          9 
```
