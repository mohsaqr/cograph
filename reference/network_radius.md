# Network Radius

Computes the radius of a network, the minimum eccentricity across all
nodes. The eccentricity of a node is its largest shortest-path distance
to any other node. Edge weights are used as distances, and directed
networks use outgoing paths.

## Usage

``` r
network_radius(x, directed = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- directed:

  Logical or NULL. Consider edge direction? Default NULL, which follows
  the directedness of the converted graph.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which accepts no arguments besides `directed`; anything else raises an
  "unused argument" error.

## Value

Numeric scalar: the network radius.

## Examples

``` r
network_radius(regulation_net)
#> [1] 0.56
```
