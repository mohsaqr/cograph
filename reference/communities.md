# Community Detection

Detects communities/clusters in networks using various algorithms.
Provides a unified interface to igraph's community detection functions
with full parameter exposure.

## Usage

``` r
communities(
  x,
  method = c("louvain", "leiden", "fast_greedy", "walktrap", "infomap",
    "label_propagation", "edge_betweenness", "leading_eigenvector", "spinglass",
    "optimal", "fluid"),
  weights = NULL,
  resolution = 1,
  directed = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- method:

  Community detection algorithm. One of:

  - `"louvain"` - Louvain modularity optimization (default, fast)

  - `"leiden"` - Leiden algorithm (improved Louvain)

  - `"fast_greedy"` - Fast greedy modularity optimization

  - `"walktrap"` - Random walk-based detection

  - `"infomap"` - Information theoretic approach

  - `"label_propagation"` - Label propagation (very fast)

  - `"edge_betweenness"` - Girvan-Newman algorithm

  - `"leading_eigenvector"` - Leading eigenvector method

  - `"spinglass"` - Spinglass simulation

  - `"optimal"` - Exact modularity optimization (slow)

  - `"fluid"` - Fluid communities algorithm

- weights:

  Edge weights. If NULL, uses edge weights from the network if
  available, otherwise unweighted. Set to NA for explicitly unweighted.

- resolution:

  Resolution parameter for modularity-based methods (louvain, leiden).
  Higher values yield more communities. Default 1.

- directed:

  Logical; whether to treat the network as directed. Default NULL
  (auto-detect).

- seed:

  Random seed for reproducibility. Only applies to stochastic algorithms
  (louvain, leiden, infomap, label_propagation, spinglass).

- ...:

  Additional parameters passed to the specific algorithm. See individual
  functions for details.

## Value

A `cograph_communities` object (extends igraph's communities class) with
components:

- membership:

  Integer vector of community assignments

- modularity:

  Modularity score of the partition

- algorithm:

  Name of the algorithm used

- names:

  Node names if available

- vcount:

  Number of nodes

## Details

**Algorithm Selection Guide:**

|                     |                                             |                 |
|---------------------|---------------------------------------------|-----------------|
| Algorithm           | Best For                                    | Time Complexity |
| louvain             | Large networks, general use                 | O(n log n)      |
| leiden              | Large networks, better quality than louvain | O(n log n)      |
| fast_greedy         | Medium networks                             | O(n² log n)     |
| walktrap            | Networks with clear community structure     | O(n² log n)     |
| infomap             | Directed networks, flow-based               | O(E)            |
| label_propagation   | Very large networks, speed critical         | O(E)            |
| edge_betweenness    | Small networks, hierarchical                | O(E² n)         |
| leading_eigenvector | Networks with dominant structure            | O(n²)           |
| spinglass           | Small networks, allows negative weights     | O(n³)           |
| optimal             | Tiny networks only (\<50 nodes)             | NP-hard         |
| fluid               | When k is known                             | O(E k)          |

## See also

[`community_louvain`](http://sonsoles.me/cograph/reference/community_louvain.md),
[`community_leiden`](http://sonsoles.me/cograph/reference/community_leiden.md),
[`community_fast_greedy`](http://sonsoles.me/cograph/reference/community_fast_greedy.md),
[`community_walktrap`](http://sonsoles.me/cograph/reference/community_walktrap.md),
[`community_infomap`](http://sonsoles.me/cograph/reference/community_infomap.md),
[`community_label_propagation`](http://sonsoles.me/cograph/reference/community_label_propagation.md),
[`community_edge_betweenness`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md),
[`community_leading_eigenvector`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md),
[`community_spinglass`](http://sonsoles.me/cograph/reference/community_spinglass.md),
[`community_optimal`](http://sonsoles.me/cograph/reference/community_optimal.md),
[`community_fluid`](http://sonsoles.me/cograph/reference/community_fluid.md)

## Examples

``` r
# Create a network with community structure
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")

  # Default (Louvain)
  comm <- cograph::communities(g)
  print(comm)

  # Walktrap
  comm2 <- cograph::communities(g, method = "walktrap")
  print(comm2)
}
#> Community structure (louvain)
#>   Number of communities: 4 
#>   Modularity: 0.4188 
#>   Community sizes: 12, 5, 11, 6 
#> Community structure (walktrap)
#>   Number of communities: 5 
#>   Modularity: 0.3532 
#>   Community sizes: 9, 7, 9, 4, 5 
```
