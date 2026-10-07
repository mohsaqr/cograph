# Remove Edges from a Network

Deletes the edges between given pairs of nodes. In an undirected network
the order of the two endpoints does not matter.

## Usage

``` r
remove_edges(
  x,
  from,
  to,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- from:

  Source nodes, by label or index.

- to:

  Target nodes, by label or index. The same length as `from`.

- keep_isolates:

  Logical. Keep nodes that end up with no edges? Default TRUE, matching
  [`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md).

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` without those edges, or the input format when
`keep_format = TRUE`. Named pairs that carry no edge are reported in a
`cograph_no_such_edge` warning.

## See also

[`add_edges`](https://sonsoles.me/cograph/reference/add_edges.md),
[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
remove_edges(regulation_net, from = "Plan", to = "Monitor")
#> Cograph network: 10 nodes, 29 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 29 / 90 (density: 32.2%)
#>   Weights: [0.050, 0.490]  |  mean: 0.270
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
