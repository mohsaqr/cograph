# Consensus Community Detection

Runs a stochastic community detection algorithm multiple times and finds
consensus communities via co-occurrence matrix thresholding. This
approach produces more robust and stable community assignments than
single runs.

## Usage

``` r
community_consensus(
  x,
  method = c("louvain", "leiden", "infomap", "label_propagation", "spinglass"),
  n_runs = 100,
  threshold = 0.5,
  seed = NULL,
  ...
)

com_consensus(
  x,
  method = c("louvain", "leiden", "infomap", "label_propagation", "spinglass"),
  n_runs = 100,
  threshold = 0.5,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- method:

  Community detection algorithm to use. Default "louvain". Must be a
  stochastic method (louvain, leiden, infomap, label_propagation,
  spinglass).

- n_runs:

  Number of times to run the algorithm. Default 100.

- threshold:

  Co-occurrence threshold for consensus. Default 0.5. Nodes that appear
  together in \>= threshold proportion of runs are placed in the same
  community.

- seed:

  Optional seed for reproducibility. If provided, the RNG state is
  initialized once before repeated runs.

- ...:

  Additional arguments passed to the community detection method.

## Value

A `cograph_communities` object with consensus membership.

## Details

The algorithm works as follows:

1.  Run the specified algorithm `n_runs` times using the current RNG
    stream

2.  Build a co-occurrence matrix counting how often each pair of nodes
    appears in the same community

3.  Normalize to proportions (0-1)

4.  Threshold to create a consensus graph (edge if co-occurrence \>=
    threshold)

5.  Run walktrap on the consensus graph to get final communities

## References

Lancichinetti, A., & Fortunato, S. (2012). Consensus clustering in
complex networks. *Scientific Reports*, 2, 336.

## See also

[`communities`](https://sonsoles.me/cograph/reference/communities.md),
[`community_louvain`](https://sonsoles.me/cograph/reference/community_louvain.md)

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")

  # Consensus from 50 Louvain runs
  cc <- community_consensus(g, method = "louvain", n_runs = 50)
  print(cc)

  # Stricter threshold for more robust communities
  cc2 <- community_consensus(g, threshold = 0.7, n_runs = 100)
}
#> Community structure (consensus_louvain)
#>   Nodes: 34  | Communities: 4  | Modularity: 0.6018 
#>   Sizes: 12, 11, 6, 5 
#> 
#>  node community
#>     1         2
#>     2         2
#>     3         2
#>     4         2
#>     5         4
#>     6         4
#>     7         4
#>     8         2
#>     9         1
#>    10         1
#>    11         4
#>    12         2
#>    13         2
#>    14         2
#>    15         1
#>    16         1
#>    17         4
#>    18         2
#>    19         1
#>    20         2
#>    21         1
#>    22         2
#>    23         1
#>    24         3
#>    25         3
#>    26         3
#>    27         1
#>    28         3
#>    29         3
#>    30         1
#>    31         1
#>    32         3
#>    33         1
#>    34         1
```
