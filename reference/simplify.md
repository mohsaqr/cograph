# Simplify a Network

Removes self-loops and merges duplicate (multi-)edges, similar to
[`igraph::simplify()`](https://r.igraph.org/reference/simplify.html).
Works on matrices, cograph_network, igraph, and tna objects.

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

  Logical. Merge duplicate edges?

- edge_attr_comb:

  How to combine weights of duplicate edges: `"sum"`, `"mean"`, `"max"`,
  `"min"`, `"first"`, or a custom function.

- ...:

  Additional arguments (currently unused).

## Value

The simplified network in the same format as the input.

## See also

[`filter_edges`](http://sonsoles.me/cograph/reference/filter_edges.md)
for conditional edge removal,
[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) which
has its own `simplify` parameter

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
#> Cograph network: 3 nodes, 2 edges ( directed )
#> Source: edgelist 
#> Data: data.frame (3 x 3) 
#>   Nodes (3): 1, 2, 3
#> Weights: 0.5 (all equal)
#> Layout: none 
simplify(net, edge_attr_comb = "sum")
#> Cograph network: 3 nodes, 2 edges ( directed )
#> Source: edgelist 
#> Data: data.frame (3 x 3) 
#>   Nodes (3): 1, 2, 3
#> Weights: 0.5 to 1 
#> Layout: none 
```
