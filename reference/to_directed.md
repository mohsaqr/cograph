# Convert an Undirected Network to Directed

Convert an Undirected Network to Directed

## Usage

``` r
to_directed(
  x,
  mode = c("mutual", "arbitrary"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- mode:

  `"mutual"` (default) creates an arc in both directions for every
  undirected edge. `"arbitrary"` keeps one arc per edge, running from
  the lower node index to the higher. For a directed input, `"mutual"`
  gives both arcs of a pair the larger of the two weights, and
  `"arbitrary"` drops every arc from a higher to a lower index.

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

A directed `cograph_network`, or the input format when
`keep_format = TRUE`.

## See also

[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md),
[`reverse_edges`](https://sonsoles.me/cograph/reference/reverse_edges.md)

## Examples

``` r
to_directed(to_undirected(regulation_net))
#> Cograph network: 10 nodes, 54 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 54 / 90 (density: 60.0%)
#>   Weights: [0.070, 0.490]  |  mean: 0.279
#>   Strongest edges:
#>     Evaluate -> Plan  0.490
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Monitor -> Share  0.490
#>     Evaluate -> Adapt  0.430
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
