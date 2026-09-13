# Community Detection

Detects communities/clusters in networks using various algorithms.
Provides a unified interface to igraph's community detection functions.

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
  cograph_network, or tna object

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

  Logical; whether edge-betweenness should treat the network as
  directed. Default NULL (auto-detect for edge-betweenness). Other
  methods use their own directed/undirected handling.

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

When called through this wrapper, methods that require undirected graphs
(`"louvain"`, `"leiden"`, `"fast_greedy"`, `"leading_eigenvector"`, and
`"fluid"`) fall back to `"walktrap"` if the input graph is directed.

**Algorithm Selection Guide:**

|  |  |  |
|----|----|----|
| Algorithm | Best For | Time Complexity |
| louvain | Large networks, general use | O(n log n) |
| leiden | Large networks, better quality than louvain | O(n log n) |
| fast_greedy | Medium networks | O(n² log n) |
| walktrap | Networks with clear community structure | O(n² log n) |
| infomap | Directed networks, flow-based | O(E) |
| label_propagation | Very large networks, speed critical | O(E) |
| edge_betweenness | Small networks, hierarchical | O(E² n) |
| leading_eigenvector | Networks with dominant structure | O(n²) |
| spinglass | Small networks, allows negative weights | O(n³) |
| optimal | Tiny networks only (\<50 nodes) | NP-hard |
| fluid | When k is known | O(E k) |

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
#>   Nodes: 34  | Communities: 4  | Modularity: 0.4151 
#>   Sizes: 11, 5, 14, 4 
#> 
#>  node community
#>     1         1
#>     2         1
#>     3         1
#>     4         1
#>     5         2
#>     6         2
#>     7         2
#>     8         1
#>     9         3
#>    10         3
#>    11         2
#>    12         1
#>    13         1
#>    14         1
#>    15         3
#>    16         3
#>    17         2
#>    18         1
#>    19         3
#>    20         1
#>    21         3
#>    22         1
#>    23         3
#>    24         3
#>    25         4
#>    26         4
#>    27         3
#>    28         3
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
