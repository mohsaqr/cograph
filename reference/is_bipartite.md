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
[`igraph::is_bipartite()`](https://r.igraph.org/reference/is_bipartite.html)
when igraph is available. Without igraph, it uses a BFS-based
two-coloring algorithm.

## Examples

``` r
# Non-square matrix is bipartite
inc <- matrix(c(1, 0, 1, 1, 1, 0), 2, 3)
cograph::is_bipartite(inc)
#> [1] TRUE

# Square bipartite-compatible adjacency
adj <- matrix(c(0, 0, 1, 1,
                0, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
cograph::is_bipartite(adj)
#> [1] TRUE

# Non-bipartite (triangle)
tri <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
cograph::is_bipartite(tri)
#> [1] FALSE
```
