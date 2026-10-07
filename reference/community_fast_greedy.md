# Fast Greedy Community Detection

Hierarchical agglomeration by greedy modularity optimization, which
produces a dendrogram of community merges. A directed graph is collapsed
to an undirected graph with summed edge weights.

## Usage

``` r
community_fast_greedy(
  x,
  weights = NULL,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)

com_fg(
  x,
  weights = NULL,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Negative weights are replaced by their absolute values.

- merges:

  Logical. Whether igraph stores the merge matrix. Default `TRUE`.

- modularity:

  Logical. Whether igraph stores the modularity scores. Default `TRUE`.

- membership:

  Logical. Whether igraph computes the membership vector. Default
  `TRUE`.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

A `cograph_communities` data frame with columns `node` and `community`.
The igraph `communities` result, including the merge dendrogram when
`merges = TRUE`, is kept in the `"igraph_result"` attribute.

## References

Clauset, A., Newman, M.E.J., & Moore, C. (2004). Finding community
structure in very large networks. *Physical Review E*, 70, 066111.

## Examples

``` r
community_fast_greedy(regulation_net)
#> Community structure (fast_greedy)
#>   Nodes: 10  | Communities: 2  | Modularity: 0.1976 
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
