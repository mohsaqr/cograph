# Modularity Vitality

Contribution of a node to the modularity of a fixed partition
(Magelinski, Bartulovic & Carley 2021): \$\$V_Q(i) = Q(G, C) - Q(G -
i,\\ C \setminus \\i\\),\$\$ the drop in Newman modularity when node
\\i\\ is deleted and the remaining nodes keep their communities.
Positive values mark community hubs (removing them weakens the modular
structure); negative values mark bridges (removing them sharpens it).
Weighted graphs use edge weights; directed graphs use the Leicht-Newman
directed modularity, as igraph does.

## Usage

``` r
centrality_modularity_vitality(x, membership = NULL, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- membership:

  Community labels, one per node (integer, factor, or character).
  Required; without it the function warns and returns `NA`. Obtain one
  from
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node. `NaN` where deleting the node
leaves a graph with no edges.

## Details

All \\n\\ vitalities are computed in closed form from one matrix
product, without recomputing modularity \\n\\ times.

## Conditions

Raises an error of class `cograph_bad_membership` when `membership` is
not one non-missing label per node.

## References

Magelinski, T., Bartulovic, M., & Carley, K. M. (2021). Measuring node
contribution to community structure with modularity vitality. IEEE
Transactions on Network Science and Engineering, 8(1), 707-723.

## See also

[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md),
[`centrality_within_module_z`](https://sonsoles.me/cograph/reference/centrality_within_module_z.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## Examples

``` r
# Two triangles joined by one bridge edge (C -- D)
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_modularity_vitality(adj, membership = c(1, 1, 1, 2, 2, 2))
#>           A           B           C           D           E           F 
#>  0.13714286  0.13714286 -0.01785714 -0.01785714  0.13714286  0.13714286 
```
