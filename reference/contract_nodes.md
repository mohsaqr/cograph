# Contract Nodes into Groups

Replaces each group of nodes with a single node whose edges aggregate
the edges of its members. The counterpart of
[`igraph::contract()`](https://r.igraph.org/reference/contract.html) and
tidygraph's `to_contracted()`, and the network form of what
[`summarize_clusters()`](https://sonsoles.me/cograph/reference/summarize_clusters.md)
computes inside an analysis object.

## Usage

``` r
contract_nodes(
  x,
  groups,
  weight = c("sum", "mean", "max", "min"),
  loops = FALSE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- groups:

  Group assignment. Either a vector with one entry per node (in node
  order), or a named list mapping group name to node labels.

- weight:

  How to aggregate the weights of the edges that fall between two
  groups: `"sum"` (default), `"mean"`, `"max"` or `"min"`.

- loops:

  Logical. Keep the within-group edges as self-loops on the contracted
  node. Default FALSE.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with one node per group, labeled by group name, or
the input format when `keep_format = TRUE`.

## See also

[`summarize_clusters`](https://sonsoles.me/cograph/reference/summarize_clusters.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md),
[`split_components`](https://sonsoles.me/cograph/reference/split_components.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

contract_nodes(adj, groups = c("left", "left", "right", "right"))
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (2): left, right
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [2.000, 2.000]  |  mean: 2.000
#>   Strongest edges:
#>     left -- right  2.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
