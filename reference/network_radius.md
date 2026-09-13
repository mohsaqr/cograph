# Network Radius

Computes the radius of a network - the minimum eccentricity across all
nodes. The eccentricity of a node is the maximum shortest path distance
to any other node. The radius is the smallest such maximum distance.

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

  Currently unused; `directed` is already an explicit argument above and
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)
  accepts no others.

## Value

Numeric: the network radius

## Examples

``` r
# Star graph: center has eccentricity 1, leaves have 2, so radius = 1
star <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), 4, 4)
network_radius(star)  # 1
#> [1] 1
```
