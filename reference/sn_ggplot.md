# Convert Network to ggplot2

Builds a ggplot object of a network that can be modified and combined
with other ggplot2 layers. Nodes are points with labels and edges are
straight segments, with arrows when the network is directed. Edges with
positive weights are green and edges with negative weights are red. The
plot uses fixed default aesthetics and does not apply styling set with
the `sn_*` functions or a theme.

## Usage

``` r
sn_ggplot(network, title = NULL)
```

## Arguments

- network:

  A `cograph_network` object, matrix, data frame edge list, igraph,
  statnet network, qgraph or tna object. A network without layout
  coordinates receives a spring layout computed with seed 42.

- title:

  Optional plot title.

## Value

A ggplot object.

## Examples

``` r
sn_ggplot(regulation_net, title = "Regulation")
```
