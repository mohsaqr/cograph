# Reorder the Nodes of a Network

Changes the order in which the nodes are stored, which is the order in
which plotting functions place them. The edges and their weights are
unchanged.

## Usage

``` r
reorder_nodes(x, order, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- order:

  Node labels or indices, in the wanted order, or one of `"label"`,
  `"degree"` or `"strength"`. `"label"` sorts alphabetically, and the
  two measures sort in decreasing order. A vector that does not name
  every node exactly once raises a `cograph_bad_selection` error.

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

A `cograph_network` with the nodes in the requested order and edge
indices remapped, or the input format when `keep_format = TRUE`.

## See also

[`rename_nodes`](https://sonsoles.me/cograph/reference/rename_nodes.md),
[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md)

## Examples

``` r
reorder_nodes(regulation_net, order = "degree")
#> Cograph network: 10 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Monitor, Plan, Create, Explore, Adapt, Reflect, ... +4 more
#>   Edges: 30 / 90 (density: 33.3%)
#>   Weights: [0.050, 0.490]  |  mean: 0.265
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
