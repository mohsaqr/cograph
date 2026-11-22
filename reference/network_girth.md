# Network Girth (Shortest Cycle Length)

Computes the girth of a network, the length of its shortest cycle, on
its simple undirected skeleton. Edge direction, self-loops and repeated
edges are ignored, so a pair of reciprocal directed edges is one edge,
not a cycle of length 2 (as `networkx.girth()` of the undirected graph).
An undirected forest has girth Inf.

## Usage

``` r
network_girth(x, ...)
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

Numeric scalar: the length of the shortest cycle, or Inf if the graph
has no cycle.

## Examples

``` r
network_girth(regulation_net)
#> [1] 3
```
