# Contract Nodes into Groups

Replaces each group of nodes with a single node whose edges aggregate
the edges of its members. The operation corresponds to
[`igraph::contract()`](https://r.igraph.org/reference/contract.html) and
tidygraph's `to_contracted()`, and it is the network form of the
aggregation computed by
[`summarize_clusters()`](https://sonsoles.me/cograph/reference/summarize_clusters.md).

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

  Group assignment. Either a vector with one entry per node, in node
  order, or a named list mapping each group name to node labels. A list
  must assign every node. Malformed input raises a
  `cograph_bad_selection` error.

- weight:

  How to aggregate the weights of the edges that fall between two
  groups. One of `"sum"` (default), `"mean"`, `"max"` or `"min"`.
  Within-group edges are aggregated the same way when `loops = TRUE`.

- loops:

  Logical. Keep the within-group edges as self-loops on the contracted
  node. Default FALSE.

- keep_format:

  Logical. If TRUE, a matrix, igraph, statnet network or tna input is
  returned in its own format. An edge-list data frame or a qgraph object
  is returned as a `cograph_network` with a
  `cograph_no_format_roundtrip` warning. Default FALSE returns a
  `cograph_network`.

- directed:

  Logical or NULL. Directedness used to read the input. NULL (default)
  detects it from the input.

## Value

A `cograph_network` with one node per group, labeled by group name, or
the input format when `keep_format = TRUE`. The groups follow the factor
levels of a vector (sorted for a character vector) or the order of a
list.

## See also

[`summarize_clusters`](https://sonsoles.me/cograph/reference/summarize_clusters.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md),
[`split_components`](https://sonsoles.me/cograph/reference/split_components.md)

## Examples

``` r
contract_nodes(regulation_net, groups = rep(c("Plan", "Act"), each = 5))
#> Cograph network: 2 nodes, 2 edges ( directed )
#> Source: matrix 
#>   Nodes (2): Act, Plan
#>   Edges: 2 / 2 (density: 100.0%)
#>   Weights: [2.600, 3.480]  |  mean: 3.040
#>   Strongest edges:
#>     Act -> Plan  3.480
#>     Plan -> Act  2.600
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
