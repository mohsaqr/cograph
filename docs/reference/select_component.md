# Select Connected Component

Select nodes belonging to a specific connected component.

## Usage

``` r
select_component(
  x,
  which = "largest",
  ...,
  .keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- which:

  Component selection:

  `"largest"`

  :   (default) The largest connected component

  Integer

  :   Component by ID

  Character

  :   Component containing the named node

- ...:

  Additional filter expressions to apply after component selection.

- .keep_edges:

  How to handle edges. Default "internal".

- keep_format:

  Logical. Keep input format? Default FALSE.

- directed:

  Logical or NULL. Auto-detect if NULL.

## Value

A cograph_network with nodes in the selected component.

## See also

[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`select_neighbors`](https://sonsoles.me/cograph/reference/select_neighbors.md)

## Examples

``` r
# Create disconnected network
adj <- matrix(0, 6, 6)
adj[1, 2] <- adj[2, 1] <- 1
adj[1, 3] <- adj[3, 1] <- 1
adj[4, 5] <- adj[5, 4] <- 1
adj[5, 6] <- adj[6, 5] <- 1
adj[4, 6] <- adj[6, 4] <- 1
rownames(adj) <- colnames(adj) <- LETTERS[1:6]

# Largest component
select_component(adj, which = "largest")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#>     A -- C  1.000
#> Layout: none 

# Component containing node "A"
select_component(adj, which = "A")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#>     A -- C  1.000
#> Layout: none 
```
