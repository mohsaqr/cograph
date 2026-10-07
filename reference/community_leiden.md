# Leiden Community Detection

The Leiden algorithm, a refinement of the Louvain algorithm that
guarantees well-connected communities. It optimizes the Constant Potts
Model (CPM) or modularity. The graph must be undirected; a directed
graph raises an igraph error.

## Usage

``` r
community_leiden(
  x,
  weights = NULL,
  resolution = 1,
  objective_function = c("CPM", "modularity"),
  beta = 0.01,
  initial_membership = NULL,
  n_iterations = 2,
  vertex_weights = NULL,
  seed = NULL,
  ...
)

com_ld(
  x,
  weights = NULL,
  resolution = 1,
  objective_function = c("CPM", "modularity"),
  beta = 0.01,
  initial_membership = NULL,
  n_iterations = 2,
  vertex_weights = NULL,
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

- resolution:

  Resolution parameter. Default 1. With the default CPM objective and
  resolution 1, a network with weights below 1 is typically split into
  single-node communities.

- objective_function:

  Optimization objective, `"CPM"` (default) or `"modularity"`.

- beta:

  Randomness parameter of the refinement step. Default 0.01.

- initial_membership:

  Initial community assignments (optional).

- n_iterations:

  Number of iterations. Default 2. A negative value iterates until the
  partition no longer changes.

- vertex_weights:

  Vertex weights for the CPM objective.

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

Traag, V.A., Waltman, L., & van Eck, N.J. (2019). From Louvain to
Leiden: guaranteeing well-connected communities. *Scientific Reports*,
9, 5233.

## Examples

``` r
community_leiden(to_undirected(regulation_net), objective_function = "modularity",
                 seed = 1)
#> Community structure (leiden)
#>   Nodes: 10  | Communities: 2  | Modularity: NA 
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
