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

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

If count_only = FALSE, data frame with from/to columns. If count_only =
TRUE, integer count.

## Examples

``` r
# Two triangles connected by single edge
adj <- matrix(0, 6, 6)
adj[1,2] <- adj[2,1] <- adj[1,3] <- adj[3,1] <- adj[2,3] <- adj[3,2] <- 1
adj[4,5] <- adj[5,4] <- adj[4,6] <- adj[6,4] <- adj[5,6] <- adj[6,5] <- 1
adj[3,4] <- adj[4,3] <- 1  # Bridge
network_bridges(adj)  # Edge 3-4
#>   from to
#> 1    3  4
network_bridges(adj, count_only = TRUE)  # 1
#> [1] 1
```
