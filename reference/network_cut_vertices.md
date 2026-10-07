# Cut Vertices (Articulation Points)

Finds nodes whose removal would disconnect the network. These are
critical nodes for network connectivity.

## Usage

``` r
network_cut_vertices(x, count_only = FALSE, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- count_only:

  Logical. If TRUE, return only the count. Default FALSE.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

If `count_only = FALSE`, a character vector of node names, or an integer
vector of node indices when the graph has no names. If
`count_only = TRUE`, an integer count.

## Examples

``` r
strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
network_cut_vertices(strong)
#> [1] "Discuss" "Reflect"
```
