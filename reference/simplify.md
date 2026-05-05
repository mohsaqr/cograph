# Simplify a Network

Removes self-loops and (where representable) merges duplicate
(multi-)edges, similar to
[`igraph::simplify()`](https://r.igraph.org/reference/simplify.html).

## Usage

``` r
simplify(x, remove_loops, remove_multiple, edge_attr_comb, ...)

# S3 method for class 'matrix'
simplify(
  x,
  remove_loops = TRUE,
  remove_multiple = TRUE,
  edge_attr_comb = "mean",
  ...
)

# S3 method for class 'cograph_network'
simplify(
  x,
  remove_loops = TRUE,
  remove_multiple = TRUE,
  edge_attr_comb = "mean",
  ...
)

# S3 method for class 'igraph'
simplify(
  x,
  remove_loops = TRUE,
  remove_multiple = TRUE,
  edge_attr_comb = "mean",
  ...
)

# S3 method for class 'tna'
simplify(
  x,
  remove_loops = TRUE,
  remove_multiple = TRUE,
  edge_attr_comb = "mean",
  ...
)

# Default S3 method
simplify(
  x,
  remove_loops = TRUE,
  remove_multiple = TRUE,
  edge_attr_comb = "mean",
  ...
)
```

## Arguments

- x:

  Network input (matrix, cograph_network, igraph, tna object).

- remove_loops:

  Logical. Remove self-loops (diagonal entries)?

- remove_multiple:

  Logical. Merge duplicate edges? No-op for matrix/tna inputs (see
  Details).

- edge_attr_comb:

  How to combine weights of duplicate edges: `"sum"`, `"mean"`, `"max"`,
  `"min"`, `"first"`, or a custom function. Ignored for matrix/tna
  inputs.

- ...:

  Additional arguments (currently unused).

## Value

The simplified network in the same format as the input.

## Details

The extent of simplification depends on the input representation:

- `matrix` and `tna`: edges are stored as an n x n weight matrix. Each
  cell (i, j) is unique by construction, so duplicate-edge merging is a
  no-op regardless of `remove_multiple` / `edge_attr_comb`; only
  self-loops (the diagonal) can be removed. Convert to `cograph_network`
  or `igraph` first if you need true duplicate aggregation.

- `cograph_network`: duplicate edges in the edge-list are merged via
  [`aggregate_duplicate_edges()`](https://sonsoles.me/cograph/reference/aggregate_duplicate_edges.md)
  using `edge_attr_comb`.

- `igraph`: delegates to
  [`igraph::simplify()`](https://r.igraph.org/reference/simplify.html).

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md)
for conditional edge removal,
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
which has its own `simplify` parameter

## Examples

``` r
# Matrix with self-loops
mat <- matrix(c(0.5, 0.3, 0, 0.3, 0.2, 0.4, 0, 0.4, 0.1), 3, 3)
rownames(mat) <- colnames(mat) <- c("A", "B", "C")
simplify(mat)
#>     A   B   C
#> A 0.0 0.3 0.0
#> B 0.3 0.0 0.4
#> C 0.0 0.4 0.0

# Edge list with duplicates
edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.3, 0.7, 0.5))
net <- cograph(edges, layout = NULL)
simplify(net)
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: edgelist 
#> Data: data.frame (3 x 3) 
#>   Nodes (3): 1, 2, 3
#> Weights: 0.5 (all equal)
#> Layout: none 
simplify(net, edge_attr_comb = "sum")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: edgelist 
#> Data: data.frame (3 x 3) 
#>   Nodes (3): 1, 2, 3
#> Weights: 0.5 to 1 
#> Layout: none 
```
