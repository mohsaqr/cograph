# Add Edges to a Network

Appends edges between existing nodes. An endpoint that is not a node of
the network raises a `cograph_bad_selection` error.

## Usage

``` r
add_edges(x, from, to, weight = 1, ..., keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- from:

  Source nodes, by label or index.

- to:

  Target nodes, by label or index. The same length as `from`.

- weight:

  Numeric weight for the new edges, length 1 or `length(from)`. Default
  1.

- ...:

  Named vectors of extra edge attributes, length 1 or `length(from)`.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the new edges, or the input format when
`keep_format = TRUE`. An edge that already exists has its weight
replaced, and a `cograph_edges_replaced` warning says how many.

## Note

When the igraph package is attached it masks this function with
[`igraph::add_edges()`](https://r.igraph.org/reference/add_edges.html),
which takes an igraph object. Use `cograph::add_edges()` to be explicit.

## See also

[`remove_edges`](https://sonsoles.me/cograph/reference/remove_edges.md),
[`add_nodes`](https://sonsoles.me/cograph/reference/add_nodes.md),
[`bind_networks`](https://sonsoles.me/cograph/reference/bind_networks.md)

## Examples

``` r
add_edges(regulation_net, from = "Share", to = "Explore", weight = 0.5)
#> Cograph network: 10 nodes, 31 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 31 / 90 (density: 34.4%)
#>   Weights: [0.050, 0.500]  |  mean: 0.273
#>   Strongest edges:
#>     Share -> Explore  0.500
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
