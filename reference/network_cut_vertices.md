# Cut Vertices (Articulation Points)

Finds nodes whose removal would disconnect the network. These are
critical nodes for network connectivity.

## Usage

``` r
network_cut_vertices(x, count_only = FALSE, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- count_only:

  Logical. If TRUE, return only the count. Default FALSE.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

If count_only = FALSE, vector of node indices (or names if graph is
named). If count_only = TRUE, integer count.

## Examples

``` r
# Bridge node connecting two components
adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
network_cut_vertices(adj)  # Node 3 is cut vertex
#> [1] 4 3
network_cut_vertices(adj, count_only = TRUE)  # 1
#> [1] 2
```
