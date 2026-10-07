# Convert a Directed Network to Undirected

Collapses each pair of opposite arcs into one undirected edge, as
[`igraph::as_undirected()`](https://r.igraph.org/reference/as_directed.html)
and tidygraph's `to_undirected()` do.

## Usage

``` r
to_undirected(
  x,
  method = c("max", "sum", "mean", "min", "mutual"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- method:

  How to combine `w[i, j]` and `w[j, i]`. One of `"max"` (default),
  `"sum"`, `"mean"`, `"min"` or `"mutual"`. The first four combine the
  two weights when both arcs exist, and an arc without a reverse arc
  keeps its own weight. `"mutual"` keeps only reciprocated pairs, at the
  smaller of the two weights.

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

An undirected `cograph_network`, or the input format when
`keep_format = TRUE`. Self-loops keep their weight. A weight of zero
means no edge, so a pair whose combined weight is exactly zero is
dropped. Under `method = "mutual"` this applies to every unreciprocated
arc, and under `"sum"` to a pair of opposite weights that cancel.
Dropped edges raise a `cograph_edges_dropped` warning.

## See also

[`to_directed`](https://sonsoles.me/cograph/reference/to_directed.md),
[`symmetrize`](https://sonsoles.me/cograph/reference/symmetrize.md)

## Examples

``` r
to_undirected(regulation_net, method = "sum")
#> Cograph network: 10 nodes, 27 edges ( undirected )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 27 / 45 (density: 60.0%)
#>   Weights: [0.070, 0.570]  |  mean: 0.295
#>   Strongest edges:
#>     Plan -- Share  0.570
#>     Monitor -- Create  0.540
#>     Plan -- Evaluate  0.490
#>     Monitor -- Share  0.490
#>     Adapt -- Evaluate  0.430
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
