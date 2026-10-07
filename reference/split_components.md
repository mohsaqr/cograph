# Split a Network into Its Connected Components

Split a Network into Its Connected Components

## Usage

``` r
split_components(x, min_size = 1L, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- min_size:

  Integer. Components with fewer nodes are dropped. Default 1 keeps
  every component, including isolated nodes.

- keep_format:

  Logical. If TRUE, each component of a matrix, igraph, statnet network
  or tna input is returned in that format. An edge-list data frame or a
  qgraph object gives `cograph_network` components with a
  `cograph_no_format_roundtrip` warning. Default FALSE.

- directed:

  Logical or NULL. Directedness used to read the input. NULL (default)
  detects it from the input.

## Value

A list of `cograph_network` objects, one per component, ordered from
largest to smallest and named `"component_1"`, `"component_2"`, and so
on. Components are weakly connected, matching
`igraph::components(mode = "weak")`. A network with no nodes gives an
empty list and a warning.

## See also

[`select_component`](https://sonsoles.me/cograph/reference/select_component.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
split_components(threshold_edges(regulation_net, minimum = 0.3))
#> $component_1
#> Cograph network: 10 nodes, 14 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 14 / 90 (density: 15.6%)
#>   Weights: [0.300, 0.490]  |  mean: 0.386
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
#> 
```
