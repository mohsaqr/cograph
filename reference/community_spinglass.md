# Spinglass Community Detection

Community detection based on the spinglass model of statistical
mechanics, optimized by simulated annealing. Negative edge weights are
supported with `implementation = "neg"`. A disconnected graph raises a
warning and only its largest component is partitioned, so the result has
one row per node of that component.

## Usage

``` r
community_spinglass(
  x,
  weights = NULL,
  vertex = NULL,
  spins = 25,
  parupdate = FALSE,
  start.temp = 1,
  stop.temp = 0.01,
  cool.fact = 0.99,
  update.rule = c("config", "random", "simple"),
  gamma = 1,
  implementation = c("orig", "neg"),
  gamma.minus = 1,
  seed = NULL,
  ...
)

com_sg(
  x,
  weights = NULL,
  vertex = NULL,
  spins = 25,
  parupdate = FALSE,
  start.temp = 1,
  stop.temp = 0.01,
  cool.fact = 0.99,
  update.rule = c("config", "random", "simple"),
  gamma = 1,
  implementation = c("orig", "neg"),
  gamma.minus = 1,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Weights are passed unchanged.

- vertex:

  Vertex whose community is searched (single community mode). `NULL`
  (default) partitions the whole network.

- spins:

  Number of spins, the upper limit on the number of communities. Default
  25.

- parupdate:

  Logical. Whether spins are updated in parallel. Default `FALSE`.

- start.temp:

  Starting temperature. Default 1.

- stop.temp:

  Stopping temperature. Default 0.01.

- cool.fact:

  Cooling factor. Default 0.99.

- update.rule:

  Null model of the update rule, one of `"config"` (default), `"random"`
  or `"simple"`.

- gamma:

  Weight of the null model term. Default 1.

- implementation:

  `"orig"` (default) or `"neg"`, the implementation that supports
  negative weights.

- gamma.minus:

  Weight of the null model term for negative edges in the `"neg"`
  implementation. Default 1.

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

Reichardt, J., & Bornholdt, S. (2006). Statistical mechanics of
community detection. *Physical Review E*, 74, 016110.

## Examples

``` r
community_spinglass(regulation_net, seed = 1)
#> Community structure (spinglass)
#>   Nodes: 10  | Communities: 2  | Modularity: -3.7742 
#>   Sizes: 5, 5 
#> 
#>        node community
#>     Explore         2
#>        Plan         1
#>     Monitor         1
#>       Adapt         2
#>     Reflect         2
#>     Discuss         2
#>  Synthesize         2
#>    Evaluate         1
#>      Create         1
#>       Share         1
```
