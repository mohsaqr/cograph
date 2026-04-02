# Network Girth (Shortest Cycle Length)

Computes the girth of a network - the length of the shortest cycle.
Returns Inf for acyclic graphs (trees, DAGs).

## Usage

``` r
network_girth(x, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

Integer: length of shortest cycle, or Inf if no cycles exist

## Examples

``` r
# Triangle has girth 3
triangle <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3)
network_girth(triangle)  # 3
#> [1] 3

# Tree has no cycles (Inf)
tree <- matrix(c(0,1,0, 1,0,1, 0,1,0), 3, 3)
network_girth(tree)  # Inf
#> [1] Inf
```
