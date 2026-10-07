# Complement of a Network

Every pair of distinct nodes that is not joined in `x` is joined in the
complement, and every joined pair is absent from it.

## Usage

``` r
complement_network(
  x,
  weight = 1,
  loops = FALSE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- weight:

  Numeric. Weight of every edge in the complement. Default 1. A weight
  of zero means no edge, so `weight = 0` raises a
  `cograph_bad_selection` error.

- loops:

  Logical. Include self-loops in the complement. Default FALSE.

- keep_format:

  Logical. If TRUE, a matrix, igraph, statnet network or tna input is
  returned in its own format. An edge-list data frame or a qgraph object
  is returned as a `cograph_network` with a
  `cograph_no_format_roundtrip` warning. Default FALSE returns a
  `cograph_network`.

- directed:

  Logical or NULL. Directedness used to read the input. NULL (default)
  detects it from the input.

## Value

A `cograph_network` holding the complement, or the input format when
`keep_format = TRUE`. Directedness is preserved.

## See also

[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md),
[`binarize`](https://sonsoles.me/cograph/reference/binarize.md)

## Examples

``` r
complement_network(threshold_edges(regulation_net, minimum = 0.2))
#> Cograph network: 10 nodes, 71 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 71 / 90 (density: 78.9%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     Plan -> Explore  1.000
#>     Monitor -> Explore  1.000
#>     Reflect -> Explore  1.000
#>     Synthesize -> Explore  1.000
#>     Evaluate -> Explore  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
