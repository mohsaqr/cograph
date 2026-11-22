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
strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
select_bridges(strong, keep_isolates = FALSE)
#> Cograph network: 4 nodes, 3 edges ( directed )
#> Source: matrix 
#>   Nodes (4): Explore, Reflect, Discuss, Synthesize
#>   Edges: 3 / 12 (density: 25.0%)
#>   Weights: [0.350, 0.420]  |  mean: 0.373
#>   Strongest edges:
#>     Synthesize -> Reflect  0.420
#>     Explore -> Reflect  0.350
#>     Discuss -> Reflect  0.350
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
