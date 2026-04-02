# Select Bridge Edges

Select edges whose removal would disconnect the graph.

## Usage

``` r
select_bridges(
  x,
  ...,
  .keep_isolates = FALSE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- ...:

  Additional filter expressions.

- .keep_isolates:

  Keep nodes with no edges? Default FALSE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with bridge edges only.

## See also

[`select_edges`](http://sonsoles.me/cograph/reference/select_edges.md),
[`select_nodes`](http://sonsoles.me/cograph/reference/select_nodes.md)

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
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#>     B -- C  1.000
#> Layout: none 
```
