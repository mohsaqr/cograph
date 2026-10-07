# Filter Edges by Metadata

Filter edges using dplyr-style expressions on any edge column. Returns a
cograph_network object by default (universal format), or optionally a
matrix, igraph, statnet network or tna object when `keep_format = TRUE`
and the input used one of those formats.

## Usage

``` r
filter_edges(
  x,
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL,
  .keep_isolates = NULL
)

subset_edges(
  x,
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL,
  .keep_isolates = NULL
)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, or tna
  object.

- ...:

  Filter expressions using any edge column (e.g., `weight > 0.5`,
  `weight > mean(weight)`, `abs(weight) > 0.3`).

- keep_isolates:

  Logical. Keep nodes that end up with no edges? Default TRUE, matching
  [`igraph::delete_edges()`](https://r.igraph.org/reference/delete_edges.html)
  and tidygraph: filtering edges does not remove nodes. Set FALSE to
  drop them, or call
  [`remove_isolates()`](https://sonsoles.me/cograph/reference/remove_isolates.md)
  afterwards.

- keep_format:

  Logical. If TRUE, matrix, igraph, statnet network and tna inputs are
  returned in that format. Default FALSE returns cograph_network
  (universal format).

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected. Only used for
  non-cograph_network inputs.

- .keep_isolates:

  Deprecated. Use `keep_isolates`.

## Value

A cograph_network object with filtered edges. If `keep_format = TRUE`,
matrix, igraph, statnet network and tna inputs are converted back to
that type. With `keep_isolates = TRUE`, a node that loses all its edges
stays in the network and a `cograph_isolates_created` warning is raised.

## See also

[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
`subset_edges`

## Examples

``` r
filter_edges(regulation_net, weight > 0.15)
#> Cograph network: 10 nodes, 22 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 22 / 90 (density: 24.4%)
#>   Weights: [0.160, 0.490]  |  mean: 0.323
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
