# Optimal Community Detection

Finds the optimal community structure by maximizing modularity exactly.
Very slow - only use for small networks (\<50 nodes).

## Usage

``` r
community_optimal(x, weights = NULL, ...)

com_op(x, weights = NULL, ...)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## Note

This is an NP-hard problem. Use only for tiny networks.

## References

Brandes, U., Delling, D., Gaertler, M., Gorke, R., Hoefer, M.,
Nikoloski, Z., & Wagner, D. (2008). On modularity clustering. *IEEE
Transactions on Knowledge and Data Engineering*, 20(2), 172-188.

## Examples

``` r
g <- igraph::make_ring(10)
comm <- community_optimal(g)
membership(comm)
#>  1  2  3  4  5  6  7  8  9 10 
#>  1  1  2  2  2  3  3  3  3  1 
net <- as_cograph(matrix(runif(25), 5, 5))
com_op(net)
#> Community structure (optimal)
#>   Nodes: 5  | Communities: 2  | Modularity: 0.2963 
#>   Sizes: 3, 2 
#> 
#>  node community
#>     1         1
#>     2         2
#>     3         2
#>     4         1
#>     5         1
```
