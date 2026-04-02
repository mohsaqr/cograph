# Network Vertex Connectivity

Computes the vertex connectivity of a network - the minimum number of
vertices that must be removed to disconnect the graph (or make it
trivial). Higher values indicate more robust network structure.

## Usage

``` r
network_vertex_connectivity(x, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Integer: minimum vertex cut size

## Examples

``` r
# Complete graph K4 has vertex connectivity 3
k4 <- matrix(1, 4, 4); diag(k4) <- 0
network_vertex_connectivity(k4)  # 3
#> [1] 3

# Path graph has vertex connectivity 1
path <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
network_vertex_connectivity(path)  # 1
#> [1] 1
```
