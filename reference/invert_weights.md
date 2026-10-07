# Invert Edge Weights (Similarity to Distance and Back)

Turns strong ties into short distances. Path-based measures treat
weights as costs, so similarity weights are inverted before such
measures are computed.

## Usage

``` r
invert_weights(
  x,
  method = c("reciprocal", "max_minus", "reflect"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- method:

  How to invert:

  `"reciprocal"`

  :   (default) `1 / w`, the standard similarity-to-distance map.

  `"max_minus"`

  :   `max(w) - w`. The strongest edge becomes zero and is therefore
      dropped; a `cograph_edges_dropped` warning says how many.

  `"reflect"`

  :   `max(w) + min(w) - w`. Reverses the order of the weights and keeps
      every edge when all weights are positive.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with inverted weights, or the input format when
`keep_format = TRUE`.

## See also

[`normalize_weights`](https://sonsoles.me/cograph/reference/normalize_weights.md),
[`shortest_paths`](https://sonsoles.me/cograph/reference/shortest_paths.md)

## Examples

``` r
invert_weights(regulation_net, method = "reciprocal")
#> Cograph network: 10 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 30 / 90 (density: 33.3%)
#>   Weights: [2.041, 20.000]  |  mean: 5.421
#>   Strongest edges:
#>     Reflect -> Explore  20.000
#>     Synthesize -> Monitor  14.286
#>     Evaluate -> Reflect  14.286
#>     Synthesize -> Plan  9.091
#>     Plan -> Monitor  7.692
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
