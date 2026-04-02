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
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](http://sonsoles.me/cograph/reference/detect_communities.md).

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
igraph::membership(comm)
#>  [1] 1 1 2 2 2 3 3 3 3 1
net <- as_cograph(matrix(runif(25), 5, 5))
com_op(net)
#> Community structure (optimal)
#>   Number of communities: 2 
#>   Modularity: 0.2432 
#>   Community sizes: 4, 1 
#>   Nodes: 5 
```
