# Spinglass Community Detection

Statistical mechanics approach using simulated annealing. Can handle
negative edge weights.

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

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- vertex:

  Vertex to find community for (single community mode). NULL for full
  partitioning.

- spins:

  Number of spins (maximum communities). Default 25.

- parupdate:

  Parallel update mode. Default FALSE.

- start.temp:

  Starting temperature. Default 1.

- stop.temp:

  Stopping temperature. Default 0.01.

- cool.fact:

  Cooling factor. Default 0.99.

- update.rule:

  Update rule: "config" (default), "random", or "simple".

- gamma:

  Gamma parameter for modularity. Default 1.

- implementation:

  "orig" (default) or "neg" (for negative weights).

- gamma.minus:

  Gamma for negative weights in "neg" implementation.

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](http://sonsoles.me/cograph/reference/detect_communities.md).

## References

Reichardt, J., & Bornholdt, S. (2006). Statistical mechanics of
community detection. *Physical Review E*, 74, 016110.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_spinglass(g)
igraph::membership(comm)
#>  [1] 3 3 3 3 2 2 2 3 4 4 2 3 3 3 4 4 2 3 4 3 4 3 4 1 1 1 4 1 1 4 4 1 4 4
net <- as_cograph(matrix(runif(25), 5, 5))
com_sg(net)
#> Community structure (spinglass)
#>   Number of communities: 1 
#>   Modularity: 0.1823 
#>   Community sizes: 5 
#>   Nodes: 5 
```
