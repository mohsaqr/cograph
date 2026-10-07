# Remove Nodes from a Network

Deletes nodes and every edge incident to them. A node that is not in the
network raises a `cograph_bad_selection` error.

## Usage

``` r
remove_nodes(x, nodes, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- nodes:

  Node labels or indices to remove.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` without those nodes and without any edge that
touched them, or the input format when `keep_format = TRUE`.

## See also

[`add_nodes`](https://sonsoles.me/cograph/reference/add_nodes.md),
[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
remove_nodes(regulation_net, nodes = "Share")
#> Cograph network: 9 nodes, 24 edges ( directed )
#> Source: matrix 
#>   Nodes (9): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +3 more
#>   Edges: 24 / 72 (density: 33.3%)
#>   Weights: [0.050, 0.490]  |  mean: 0.250
#>   Strongest edges:
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#>     Create -> Evaluate  0.390
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
