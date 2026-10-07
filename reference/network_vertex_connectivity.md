# Network Vertex Connectivity

Computes the vertex connectivity of a network, the minimum number of
vertices whose removal disconnects the graph or leaves a single vertex.
Higher values indicate a more robust structure.

## Usage

``` r
network_vertex_connectivity(x, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

Numeric scalar: the minimum vertex cut size, or `NA` when igraph cannot
compute it.

## Examples

``` r
network_vertex_connectivity(regulation_net)
#> [1] 1
```
