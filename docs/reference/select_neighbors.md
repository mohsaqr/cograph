# Select Node Neighbors (Ego Network)

Select nodes within a specified distance from focal nodes.

## Usage

``` r
select_neighbors(
  x,
  of,
  order = 1L,
  ...,
  .keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- of:

  Character or integer. Focal node(s) by name or index.

- order:

  Integer. Neighborhood order (1 = direct neighbors). Default 1.

- ...:

  Additional filter expressions to apply after neighborhood selection.

- .keep_edges:

  How to handle edges. Default "internal".

- keep_format:

  Logical. Keep input format? Default FALSE.

- directed:

  Logical or NULL. Auto-detect if NULL.

## Value

A cograph_network with nodes in the neighborhood.

## See also

[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`select_component`](https://sonsoles.me/cograph/reference/select_component.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Direct neighbors of A
select_neighbors(adj, of = "A")
#> Cograph network: 3 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 3 / 3 (density: 100.0%)
#>   Weights: [0.300, 0.800]  |  mean: 0.533
#>   Strongest edges:
#>     A -- C  0.800
#>     A -- B  0.500
#>     B -- C  0.300
#> Layout: none 

# Neighbors up to 2 hops
select_neighbors(adj, of = "A", order = 2)
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [0.300, 0.800]  |  mean: 0.520
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     C -- D  0.400
#>     B -- C  0.300
#> Layout: none 
```
