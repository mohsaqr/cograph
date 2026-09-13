# Add Nodes to a Network

Add Nodes to a Network

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
adj <- matrix(c(0, 1, 1, 0), 2, 2)
rownames(adj) <- colnames(adj) <- c("A", "B")

add_nodes(adj, labels = c("C", "D"))
#> Cograph network: 4 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 1 / 6 (density: 16.7%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
add_nodes(adj, labels = "C", group = "new")
#> Cograph network: 3 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 1 / 3 (density: 33.3%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
