# Select Connected Component

Select nodes belonging to a specific connected component.

## Usage

``` r
select_component(
  x,
  which = "largest",
  ...,
  keep_edges = c("internal", "none"),
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

- keep_edges:

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
select_component(regulation_net, which = "largest")
#> Cograph network: 10 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 30 / 90 (density: 33.3%)
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
