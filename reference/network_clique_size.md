# Largest Clique Size

Computes the size of the largest clique (complete subgraph) in the
network, also called the clique number or omega of the graph.

## Usage

``` r
network_clique_size(x, ...)
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

Numeric scalar: the size of the largest clique.

## Details

A clique is defined on undirected ties, so a directed network is read
with each pair of nodes joined when either direction is present, and
loops and repeated edges are dropped before counting.

## Examples

``` r
network_clique_size(regulation_net)
#> [1] 4
```
