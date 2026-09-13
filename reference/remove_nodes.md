# Remove Nodes from a Network

Remove Nodes from a Network

## Usage

``` r
remove_nodes(x, nodes, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- nodes:

  Node labels or indices to remove.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` without those nodes and without any edge that
touched them, or the input format when `keep_format = TRUE`.

## See also

[`add_nodes`](https://sonsoles.me/cograph/reference/add_nodes.md),
[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

remove_nodes(adj, nodes = "B")
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (2): A, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- C  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
