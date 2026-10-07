# Walktrap Community Detection

Detects communities from short random walks. Nodes in the same community
have short random walk distances.

## Usage

``` r
community_walktrap(
  x,
  weights = NULL,
  steps = 4,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)

com_wt(
  x,
  weights = NULL,
  steps = 4,
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

- steps:

  Length of the random walks. Default 4.

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
See
[`communities`](https://sonsoles.me/cograph/reference/communities.md)
for its attributes.

## References

Pons, P., & Latapy, M. (2006). Computing communities in large networks
using random walks. *Journal of Graph Algorithms and Applications*,
10(2), 191-218.

## Examples

``` r
community_walktrap(regulation_net, steps = 4)
#> Community structure (walktrap)
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
