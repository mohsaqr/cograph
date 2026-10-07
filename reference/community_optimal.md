# Optimal Community Detection

Finds the partition with maximum modularity by exact optimization. Exact
modularity maximization is NP-hard, so the computation is feasible only
for small networks. A network with more than 50 nodes raises a warning.

## Usage

``` r
community_optimal(x, weights = NULL, ...)

com_op(x, weights = NULL, ...)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Weights are passed unchanged.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

A `cograph_communities` data frame with columns `node` and `community`.
See
[`communities`](https://sonsoles.me/cograph/reference/communities.md)
for its attributes.

## References

Brandes, U., Delling, D., Gaertler, M., Gorke, R., Hoefer, M.,
Nikoloski, Z., & Wagner, D. (2008). On modularity clustering. *IEEE
Transactions on Knowledge and Data Engineering*, 20(2), 172-188.

## Examples

``` r
community_optimal(regulation_net)
#> Community structure (optimal)
#>   Nodes: 10  | Communities: 2  | Modularity: 0.2033 
#>   Sizes: 5, 5 
#> 
#>        node community
#>     Explore         1
#>        Plan         2
#>     Monitor         2
#>       Adapt         1
#>     Reflect         1
#>     Discuss         1
#>  Synthesize         1
#>    Evaluate         2
#>      Create         2
#>       Share         2
```
