# Reverse Edge Direction

Swaps the endpoints of every edge, which transposes the weight matrix.
In a transition network the reversed arcs show where each transition
came from. Additional edge columns are kept.

## Usage

``` r
reverse_edges(x, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

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

A `cograph_network` with every edge reversed, or the input format when
`keep_format = TRUE`. An undirected network is returned unchanged, with
a `cograph_no_effect` warning.

## See also

[`to_directed`](https://sonsoles.me/cograph/reference/to_directed.md),
[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md)

## Examples

``` r
reverse_edges(regulation_net)
#> Cograph network: 10 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 30 / 90 (density: 33.3%)
#>   Weights: [0.050, 0.490]  |  mean: 0.265
#>   Strongest edges:
#>     Evaluate -> Plan  0.490
#>     Monitor -> Share  0.490
#>     Adapt -> Evaluate  0.430
#>     Reflect -> Synthesize  0.420
#>     Discuss -> Plan  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
