# Infomap Community Detection

Information-theoretic community detection based on random walks. The
partition minimizes the map equation, the description length of a random
walk on the network.

## Usage

``` r
community_infomap(
  x,
  weights = NULL,
  v.weights = NULL,
  nb.trials = 10,
  modularity = TRUE,
  seed = NULL,
  ...
)

com_im(
  x,
  weights = NULL,
  v.weights = NULL,
  nb.trials = 10,
  modularity = TRUE,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Negative weights are replaced by their absolute values.

- v.weights:

  Vertex weights (teleportation weights).

- nb.trials:

  Number of optimization trials. Default 10.

- modularity:

  Logical. Whether modularity is computed. Default `TRUE`.

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

Rosvall, M., & Bergstrom, C.T. (2008). Maps of random walks on complex
networks reveal community structure. *PNAS*, 105(4), 1118-1123.

## Examples

``` r
community_infomap(regulation_net, nb.trials = 10, seed = 1)
#> Community structure (infomap)
#>   Nodes: 10  | Communities: 1  | Modularity: 0 
#>   Sizes: 10 
#> 
#>        node community
#>     Explore         1
#>        Plan         1
#>     Monitor         1
#>       Adapt         1
#>     Reflect         1
#>     Discuss         1
#>  Synthesize         1
#>    Evaluate         1
#>      Create         1
#>       Share         1
```
