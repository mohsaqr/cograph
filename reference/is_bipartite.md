# Check if a Matrix Could Be Bipartite

Tests whether a matrix could represent a bipartite incidence matrix. A
non-square matrix is considered bipartite by default. For square
matrices, checks whether the corresponding graph has bipartite structure
(i.e., nodes can be partitioned into two groups with edges only between
groups).

## Usage

``` r
is_bipartite(x)
```

## Arguments

- x:

  A numeric matrix.

## Value

Logical. `TRUE` if the matrix could represent a bipartite network,
`FALSE` otherwise.

## Details

For non-square matrices, returns `TRUE` since they naturally represent
two-mode data (rows and columns are distinct node types).

For square matrices, the function checks whether the corresponding
undirected graph is bipartite by attempting a two-coloring via
[`igraph::bipartite_mapping()`](https://r.igraph.org/reference/bipartite_mapping.html)
when igraph is available. Without igraph, it uses a breadth-first
two-coloring. Positive entries define the edges, edge direction is
ignored, and the diagonal (self-loops) is dropped before the check.

## Examples

``` r
inc <- matrix(c(1, 0, 1, 1, 1, 0), 2, 3)
cograph::is_bipartite(inc)
#> [1] TRUE
```
