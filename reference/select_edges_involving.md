# Select Edges Involving Nodes

Select edges where at least one endpoint is in the specified node set.

## Usage

``` r
select_edges_involving(
  x,
  nodes,
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- nodes:

  Character or integer. Node names or indices.

- ...:

  Additional filter expressions.

- keep_isolates:

  Keep nodes that end up with no edges? Default TRUE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with edges involving the specified nodes.

## See also

[`select_edges`](https://sonsoles.me/cograph/reference/select_edges.md),
[`select_edges_between`](https://sonsoles.me/cograph/reference/select_edges_between.md)

## Examples

``` r
select_edges_involving(regulation_net, nodes = "Plan", keep_isolates = FALSE)
#> Cograph network: 7 nodes, 7 edges ( directed )
#> Source: matrix 
#>   Nodes (7): Plan, Monitor, Discuss, Synthesize, Evaluate, Create, Share
#>   Edges: 7 / 42 (density: 16.7%)
#>   Weights: [0.110, 0.490]  |  mean: 0.271
#>   Strongest edges:
#>     Plan -> Evaluate  0.490
#>     Plan -> Discuss  0.400
#>     Plan -> Share  0.360
#>     Share -> Plan  0.210
#>     Plan -> Create  0.200
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
