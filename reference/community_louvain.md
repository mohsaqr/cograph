# Louvain Community Detection

Multi-level modularity optimization with the Louvain algorithm. The
graph must be undirected; a directed graph raises an igraph error, so a
directed network is converted first, for example with
[`to_undirected()`](https://sonsoles.me/cograph/reference/to_undirected.md).

## Usage

``` r
community_louvain(x, weights = NULL, resolution = 1, seed = NULL, ...)

com_lv(x, weights = NULL, resolution = 1, seed = NULL, ...)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Negative weights are replaced by their absolute values.

- resolution:

  Resolution parameter. Higher values yield more communities. Default 1
  (standard modularity).

- seed:

  Random seed for reproducibility. Default NULL.

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

Blondel, V.D., Guillaume, J.L., Lambiotte, R., & Lefebvre, E. (2008).
Fast unfolding of communities in large networks. *Journal of Statistical
Mechanics*, P10008.

## Examples

``` r
community_louvain(to_undirected(regulation_net), seed = 1)
#> Community structure (louvain)
#>   Nodes: 10  | Communities: 2  | Modularity: 0.1852 
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
