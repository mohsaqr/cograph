# Bridge Edges

Finds edges whose removal would disconnect the network. These are
critical edges for network connectivity.

## Usage

``` r
network_bridges(x, count_only = FALSE, ...)
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

If `count_only = FALSE`, a data frame with one row per bridge and
columns `from` and `to` (node names, or integer indices when the graph
has no names). If `count_only = TRUE`, an integer count.

## Examples

``` r
strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
network_bridges(strong)
#>         from      to
#> 1    Discuss Reflect
#> 2 Synthesize Reflect
#> 3    Explore Reflect
```
