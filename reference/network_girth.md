# Network Girth (Shortest Cycle Length)

Computes the girth of a network, the length of its shortest cycle. Edge
direction, self-loops and repeated undirected edges are ignored. A pair
of reciprocal directed edges counts as a cycle of length 2, so a
directed network with any mutual tie has girth 2. An undirected forest
has girth Inf.

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
