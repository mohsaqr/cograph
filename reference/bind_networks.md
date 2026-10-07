# Combine Two Networks

Aligns two networks on node labels and combines their edges.

## Usage

``` r
bind_networks(
  x,
  y,
  method = c("union", "intersection", "difference"),
  weight = c("sum", "mean", "max", "min", "first"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x, y:

  Network inputs.

- method:

  How to combine the edge sets:

  `"union"`

  :   (default) every edge of either network, over the union of the node
      sets

  `"intersection"`

  :   only edges present in both, over the nodes common to both

  `"difference"`

  :   edges of `x` that are not in `y`, over the nodes of `x`

- weight:

  How to combine the weights of an edge present in both: `"sum"`
  (default), `"mean"`, `"max"`, `"min"`, or `"first"` (keep `x`'s
  weight).

- keep_format:

  Logical. Return `x`'s format when TRUE.

- directed:

  Logical or NULL. If NULL (default), the result is directed when either
  input is.

## Value

A `cograph_network` over the combined node set, or `x`'s format when
`keep_format = TRUE`. Nodes are ordered with `x`'s first, then any node
only `y` has.

## See also

[`add_edges`](https://sonsoles.me/cograph/reference/add_edges.md),
[`plot_difference`](https://sonsoles.me/cograph/reference/plot_difference.md)

## Examples

``` r
bind_networks(regulation_net, t(regulation_net))
#> Cograph network: 10 nodes, 54 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 54 / 90 (density: 60.0%)
#>   Weights: [0.070, 0.570]  |  mean: 0.295
#>   Strongest edges:
#>     Share -> Plan  0.570
#>     Plan -> Share  0.570
#>     Create -> Monitor  0.540
#>     Monitor -> Create  0.540
#>     Evaluate -> Plan  0.490
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
