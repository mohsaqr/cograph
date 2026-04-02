# Largest Clique Size

Finds the size of the largest clique (complete subgraph) in the network.
Also known as the clique number or omega of the graph.

## Usage

``` r
network_clique_size(x, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Integer: size of the largest clique

## Examples

``` r
# Triangle embedded in larger graph
adj <- matrix(c(0,1,1,1, 1,0,1,0, 1,1,0,0, 1,0,0,0), 4, 4)
network_clique_size(adj)  # 3
#> [1] 3
```
