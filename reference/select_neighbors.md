# Select Node Neighbors (Ego Network)

Select nodes within a specified distance from focal nodes.

## Usage

``` r
select_neighbors(
  x,
  of,
  order = 1L,
  ...,
  keep_edges = c("internal", "none"),
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

- keep_edges:

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
select_neighbors(regulation_net, of = "Plan")
#> Cograph network: 7 nodes, 15 edges ( directed )
#> Source: matrix 
#>   Nodes (7): Plan, Monitor, Discuss, Synthesize, Evaluate, Create, Share
#>   Edges: 15 / 42 (density: 35.7%)
#>   Weights: [0.070, 0.490]  |  mean: 0.273
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Plan -> Discuss  0.400
#>     Create -> Evaluate  0.390
#>     Monitor -> Create  0.370
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
