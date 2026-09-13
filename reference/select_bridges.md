# Select Bridge Edges

Select edges whose removal would disconnect the graph.

## Usage

``` r
select_bridges(
  x,
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- ...:

  Additional filter expressions.

- keep_isolates:

  Keep nodes that end up with no edges? Default TRUE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with bridge edges only.

## See also

[`select_edges`](https://sonsoles.me/cograph/reference/select_edges.md),
[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md)

## Examples

``` r
# Create network with bridge
adj <- matrix(0, 5, 5)
adj[1, 2] <- adj[2, 1] <- 1
adj[2, 3] <- adj[3, 2] <- 1  # Bridge
adj[3, 4] <- adj[4, 3] <- 1
adj[4, 5] <- adj[5, 4] <- 1
adj[3, 5] <- adj[5, 3] <- 1
rownames(adj) <- colnames(adj) <- LETTERS[1:5]

select_bridges(adj)
#> Warning: 2 node(s) have no edges left. Nodes are kept; call remove_isolates() to drop them.
#> Cograph network: 5 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (5): A, B, C, D, E
#>   Edges: 2 / 10 (density: 20.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#>     B -- C  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
