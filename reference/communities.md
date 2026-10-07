# Community Detection

Detects communities in a network with one of the community detection
algorithms of igraph. Each method calls the matching `community_*()`
function.

## Usage

``` r
communities(
  x,
  method = c("louvain", "leiden", "fast_greedy", "walktrap", "infomap",
    "label_propagation", "edge_betweenness", "leading_eigenvector", "spinglass",
    "optimal", "fluid"),
  community = NULL,
  weights = NULL,
  resolution = 1,
  directed = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, CographNetwork,
  cograph_network, or tna object.

- method:

  Community detection algorithm. One of `"louvain"` (default; Louvain
  modularity optimization), `"leiden"` (Leiden algorithm),
  `"fast_greedy"` (greedy modularity optimization), `"walktrap"` (random
  walks), `"infomap"` (map equation), `"label_propagation"` (label
  propagation), `"edge_betweenness"` (Girvan-Newman),
  `"leading_eigenvector"` (leading eigenvector of the modularity
  matrix), `"spinglass"` (spinglass model), `"optimal"` (exact
  modularity maximization) or `"fluid"` (fluid communities).

- community:

  Optional integer or character vector. If supplied, the returned data
  frame is filtered to rows whose `community` column matches one of the
  given values. Default `NULL` (keep all communities).

- weights:

  Edge weights. `NULL` (default) uses the edge weights of the network
  when present and otherwise runs unweighted. `NA` runs unweighted.

- resolution:

  Resolution parameter of the louvain and leiden methods. For louvain,
  higher values yield more communities. Default 1.

- directed:

  Logical. Whether the edge betweenness method treats the network as
  directed. `NULL` (default) uses the direction of the network. The
  other methods ignore it.

- seed:

  Random seed for reproducibility. It applies to the stochastic methods
  (louvain, leiden, infomap, label_propagation, spinglass).

- ...:

  Additional arguments passed to the `community_*()` function of the
  chosen method, for example `no.of.communities` for `"fluid"`.

## Value

A `cograph_communities` data frame with one row per node and the columns

- node:

  Node label (character).

- community:

  Community number (numeric).

The attributes `"algorithm"` (method name), `"modularity"` (modularity
of the partition, `NA` when igraph does not compute it), `"network"`
(the input `x`) and `"igraph_result"` (the igraph `communities` object)
hold the metadata. When `community` is supplied, only the matching rows
are kept.

## Details

The louvain, leiden, fast_greedy, leading_eigenvector and fluid methods
require an undirected graph. For a directed input this function prints a
message and runs `"walktrap"` instead. Called directly,
[`community_louvain()`](https://sonsoles.me/cograph/reference/community_louvain.md)
and
[`community_leiden()`](https://sonsoles.me/cograph/reference/community_leiden.md)
raise an igraph error on a directed graph, while
[`community_fast_greedy()`](https://sonsoles.me/cograph/reference/community_fast_greedy.md),
[`community_leading_eigenvector()`](https://sonsoles.me/cograph/reference/community_leading_eigenvector.md)
and
[`community_fluid()`](https://sonsoles.me/cograph/reference/community_fluid.md)
collapse it to an undirected graph with summed weights.

Negative edge weights are replaced by their absolute values for all
methods except spinglass and optimal, which receive the weights as they
are.

|                     |                                                       |
|---------------------|-------------------------------------------------------|
| Method              | Typical use                                           |
| louvain             | Large undirected networks                             |
| leiden              | Large undirected networks, well-connected communities |
| fast_greedy         | Medium-sized networks, hierarchical merges            |
| walktrap            | Directed or undirected networks, hierarchical merges  |
| infomap             | Directed networks with flow structure                 |
| label_propagation   | Very large networks                                   |
| edge_betweenness    | Small networks, hierarchical splits                   |
| leading_eigenvector | Undirected networks, hierarchical splits              |
| spinglass           | Small connected networks, negative weights            |
| optimal             | Networks of at most about 50 nodes                    |
| fluid               | Connected networks with a known number of communities |

## Printing and plotting

Printing the result shows the algorithm, the number of nodes and
communities, the modularity, the community sizes and the node table. The
result is itself a data frame.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## See also

[`community_louvain`](https://sonsoles.me/cograph/reference/community_louvain.md),
[`community_leiden`](https://sonsoles.me/cograph/reference/community_leiden.md),
[`community_fast_greedy`](https://sonsoles.me/cograph/reference/community_fast_greedy.md),
[`community_walktrap`](https://sonsoles.me/cograph/reference/community_walktrap.md),
[`community_infomap`](https://sonsoles.me/cograph/reference/community_infomap.md),
[`community_label_propagation`](https://sonsoles.me/cograph/reference/community_label_propagation.md),
[`community_edge_betweenness`](https://sonsoles.me/cograph/reference/community_edge_betweenness.md),
[`community_leading_eigenvector`](https://sonsoles.me/cograph/reference/community_leading_eigenvector.md),
[`community_spinglass`](https://sonsoles.me/cograph/reference/community_spinglass.md),
[`community_optimal`](https://sonsoles.me/cograph/reference/community_optimal.md),
[`community_fluid`](https://sonsoles.me/cograph/reference/community_fluid.md)

## Examples

``` r
communities(regulation_net, method = "walktrap")
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
