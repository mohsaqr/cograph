# Remove Isolated Nodes

Removes every node with no edges. Edge filters such as
[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md)
keep all nodes, and this function removes the isolates that such a
filter leaves behind.

## Usage

``` r
remove_isolates(x, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, tna, or an
  edge-list data frame.

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

A `cograph_network` with the isolated nodes removed (or the input format
when `keep_format = TRUE`). The remaining nodes keep their order, and
edge indices are remapped to the new node numbering.

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`split_components`](https://sonsoles.me/cograph/reference/split_components.md),
[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md)

## Examples

``` r
remove_isolates(threshold_edges(regulation_net, minimum = 0.3))
#> Cograph network: 10 nodes, 14 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 14 / 90 (density: 15.6%)
#>   Weights: [0.300, 0.490]  |  mean: 0.386
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
