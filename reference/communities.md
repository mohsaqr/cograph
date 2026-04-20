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

- community:

  Optional integer or character vector. If supplied, the returned data
  frame is filtered to rows whose `community` column matches one of the
  given values. Default `NULL` (keep all communities).

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

A tidy `cograph_communities` data frame with columns:

- node:

  Node label (character)

- community:

  Community assignment (integer)

Metadata stored as attributes: `"algorithm"`, `"modularity"`,
`"network"` (original input), `"igraph_result"`.

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
#>   Nodes: 34  | Communities: 4  | Modularity: 0.3969 
#>   Sizes: 8, 9, 11, 6 
#> 
#>  node community
#>     1         1
#>     2         2
#>     3         2
#>     4         2
#>     5         1
#>     6         1
#>     7         1
#>     8         2
#>     9         3
#>    10         2
#>    11         1
#>    12         1
#>    13         1
#>    14         2
#>    15         3
#>    16         3
#>    17         1
#>    18         2
#>    19         3
#>    20         2
#>    21         3
#>    22         2
#>    23         3
#>    24         4
#>    25         4
#>    26         4
#>    27         3
#>    28         4
#>    29         4
#>    30         3
#>    31         3
#>    32         4
#>    33         3
#>    34         3
#> Community structure (walktrap)
#>   Nodes: 34  | Communities: 5  | Modularity: 0.3532 
#>   Sizes: 9, 7, 9, 4, 5 
#> 
#>  node community
#>     1         1
#>     2         1
#>     3         2
#>     4         1
#>     5         5
#>     6         5
#>     7         5
#>     8         1
#>     9         2
#>    10         2
#>    11         5
#>    12         1
#>    13         1
#>    14         2
#>    15         3
#>    16         3
#>    17         5
#>    18         1
#>    19         3
#>    20         1
#>    21         3
#>    22         1
#>    23         3
#>    24         4
#>    25         4
#>    26         4
#>    27         3
#>    28         4
#>    29         2
#>    30         3
#>    31         2
#>    32         2
#>    33         3
#>    34         3
```
