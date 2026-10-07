# Modularity Vitality

Modularity vitality (Magelinski, Bartulovic and Carley 2021) is the drop
in Newman modularity of a fixed partition \\C\\ when a node is deleted
and the remaining nodes keep their communities: \$\$V_Q(i) = Q(G, C) -
Q(G - i, C \setminus \\i\\).\$\$ Positive values mark community hubs and
negative values mark bridges between communities.

## Usage

``` r
centrality_modularity_vitality(x, membership = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- membership:

  Community labels, one per node (integer, factor or character), for
  example from
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`) and
  `loops` (keep self-loops, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are used. A directed network uses the Leicht-Newman
directed modularity, and the values equal those obtained by deleting
each node and recomputing
[`igraph::modularity()`](https://r.igraph.org/reference/modularity.igraph.html).
Self-loops enter the modularity, and `loops = FALSE` drops them. A node
whose deletion leaves a graph with no edges returns `NaN`. Without
`membership` the function raises an unclassed warning and returns `NA`
for every node. A `membership` that is not one non-missing label per
node raises an error of class `cograph_bad_membership`.

## References

Magelinski, T., Bartulovic, M., & Carley, K. M. (2021). Measuring node
contribution to community structure with modularity vitality. IEEE
Transactions on Network Science and Engineering, 8(1), 707-723.

## See also

[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md),
[`centrality_within_module_z`](https://sonsoles.me/cograph/reference/centrality_within_module_z.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_modularity_vitality(regulation_net,
                               membership = rep(1:2, each = 5))
#>      Explore         Plan      Monitor        Adapt      Reflect      Discuss 
#>  0.053967672 -0.101796335  0.004415560  0.004479664  0.039233478 -0.037692695 
#>   Synthesize     Evaluate       Create        Share 
#> -0.020910192  0.006336680  0.063488155 -0.030896476 
```
