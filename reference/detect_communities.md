# Detect Communities in a Network

Detects communities (clusters) in a network using various community
detection algorithms. Returns a data frame with node-community
assignments.

## Usage

``` r
detect_communities(x, method = "louvain", directed = NULL, weights = TRUE)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- method:

  Community detection algorithm to use. One of:

  - `"louvain"`: Louvain modularity optimization (default)

  - `"walktrap"`: Walktrap algorithm based on random walks

  - `"fast_greedy"`: Fast greedy modularity optimization

  - `"label_prop"`: Label propagation algorithm

  - `"infomap"`: Infomap algorithm based on information flow

  - `"leiden"`: Leiden algorithm, run with the defaults of
    [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html).
    Its default constant Potts model objective can place every node in
    its own community.

  `"louvain"`, `"leiden"` and `"fast_greedy"` require an undirected
  graph. A directed network is collapsed to an undirected one for these
  methods, with the weights of reciprocal edges averaged, and a message
  is issued for `"louvain"` and `"leiden"`. The `"louvain"`, `"leiden"`,
  `"label_prop"` and `"infomap"` methods use random numbers, so
  [`set.seed()`](https://rdrr.io/r/base/Random.html) makes their result
  reproducible.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- weights:

  Logical. Use edge weights for community detection. Default TRUE.

## Value

A `cograph_communities` object, which inherits from `data.frame` and has
one row per node with columns:

- `node`: Node labels/names

- `community`: Numeric community membership

The algorithm name, the igraph community object, the modularity and the
input network are carried as attributes for the `print`, `plot` and
`modularity` methods.

## Examples

``` r
detect_communities(regulation_net, method = "walktrap")
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
