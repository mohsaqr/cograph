# Add Nodes to a Network

Appends new nodes, identified by label, to a network. A label that
already exists raises a `cograph_bad_selection` error.

## Usage

``` r
add_nodes(x, labels, ..., keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- labels:

  Character vector of labels for the new nodes.

- ...:

  Named vectors of node attributes for the new nodes, each of length 1
  (recycled) or `length(labels)`. Columns the network does not already
  have are created and filled with `NA` for the existing nodes.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the new nodes appended (isolated until edges
are added), or the input format when `keep_format = TRUE`.

## See also

[`remove_nodes`](https://sonsoles.me/cograph/reference/remove_nodes.md),
[`add_edges`](https://sonsoles.me/cograph/reference/add_edges.md),
[`mutate_nodes`](https://sonsoles.me/cograph/reference/mutate_nodes.md)

## Examples

``` r
add_nodes(regulation_net, labels = "Revise")
#> Cograph network: 11 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (11): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +5 more
#>   Edges: 30 / 110 (density: 27.3%)
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
