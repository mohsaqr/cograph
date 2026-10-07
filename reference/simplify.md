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

  Logical. Remove self-loops (diagonal entries)? Default `TRUE`.

- remove_multiple:

  Logical. Merge duplicate edges? Default `TRUE`. Ignored for matrix and
  tna inputs (see Details).

- edge_attr_comb:

  How to combine weights of duplicate edges: `"sum"`, `"mean"`
  (default), `"max"`, `"min"`, `"first"`, or a custom function. Ignored
  for matrix and tna inputs.

- ...:

  Additional arguments (currently unused).

## Value

The simplified network, in the same format and class as the input
(matrix in / matrix out, `cograph_network` in / `cograph_network` out,
and so on). The default method raises an error for any other class.

## Details

The extent of simplification depends on the input representation:

- `matrix` and `tna`: edges are stored as an n x n weight matrix. Each
  cell (i, j) is unique by construction, so duplicate-edge merging has
  no effect and `remove_multiple` and `edge_attr_comb` are ignored. Only
  self-loops (the diagonal) are removed. Duplicate aggregation requires
  a `cograph_network` or `igraph` input.

- `cograph_network`: duplicate edges in the edge list are merged, and
  their weights are combined with `edge_attr_comb`.

- `igraph`: delegates to
  [`igraph::simplify()`](https://r.igraph.org/reference/simplify.html).
  The `weight` attribute is combined with `edge_attr_comb` and other
  edge attributes are dropped.

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md)
for conditional edge removal,
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
which has its own `simplify` parameter

## Examples

``` r
# igraph also exports simplify(); qualify the call when both are loaded.
cograph::simplify(cograph(student_interactions), edge_attr_comb = "sum")
#> Cograph network: 34 nodes, 220 edges ( directed )
#> Source: edgelist 
#> Data: data.frame (389 x 2) 
#>   Nodes (34): Ac, Ad, Fi, Ik, Vx, Rt, ... +28 more
#> Weights: 1 to 14 
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
