# Remove Isolated Nodes

Drops every node with no edges. Filtering edges deliberately keeps nodes
(see
[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md)),
so this is the explicit way to prune the isolates a filter left behind.

## Usage

``` r
remove_isolates(x, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, tna, or an
  edge-list data frame.

- keep_format:

  Logical. If TRUE, matrix, igraph, statnet network and tna inputs are
  returned in that format. Default FALSE returns a cograph_network.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the isolated nodes removed (or the input format
when `keep_format = TRUE`). Node order is otherwise preserved and edge
indices are remapped to the new node numbering.

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`split_components`](https://sonsoles.me/cograph/reference/split_components.md),
[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md)

## Examples

``` r
adj <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
adj["A", "B"] <- adj["B", "A"] <- 1

# C and D have no edges
remove_isolates(adj)
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (2): A, B
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
