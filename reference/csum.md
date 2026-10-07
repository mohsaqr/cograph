# Cluster Summary Statistics

Aggregates node-level network weights to cluster-level summaries. The
result holds the macro (cluster-to-cluster) network and one network of
within-cluster connections per cluster.

## Usage

``` r
csum(
  x,
  clusters = NULL,
  method = c("sum", "mean", "median", "max", "min", "density", "geomean"),
  type = c("tna", "cooccurrence", "semi_markov", "raw"),
  directed = TRUE,
  compute_within = TRUE
)
```

## Arguments

- x:

  Network input. Accepts multiple formats:

  matrix

  :   Numeric adjacency/weight matrix. Row and column names are used as
      node labels. Values represent edge weights (e.g., transition
      counts, co-occurrence frequencies, or probabilities).

  cograph_network

  :   A cograph network object. Its weight matrix is used, and clusters
      can be auto-detected from node attributes.

  tna

  :   A tna object from the tna package. Its weight matrix is used and
      its sequence data are kept in `macro$data`.

  cluster_summary

  :   Returned unchanged.

- clusters:

  Cluster/group assignments for nodes. Accepts multiple formats:

  NULL

  :   (default) Auto-detect from a cograph_network. The first node
      column named 'clusters', 'cluster', 'groups' or 'group' is used,
      then a 'cluster', 'group' or 'layer' column of the node groups. An
      error is raised when none is found, and for any other input
      `clusters` must be supplied.

  vector

  :   Cluster membership for each node, in the same order as the matrix
      rows/columns. Can be numeric (1, 2, 3) or character ("A", "B").
      Cluster names are the unique values, sorted for a numeric vector
      and in order of appearance for a character vector or factor.
      Example: `c(1, 1, 2, 2, 3, 3)` assigns first two nodes to cluster
      1.

  data.frame

  :   A data frame where the first column contains node names and the
      second column contains group/cluster names. Example:
      `data.frame(node = c("A", "B", "C"), group = c("G1", "G1", "G2"))`

  named list

  :   Explicit mapping of cluster names to node labels. List names
      become cluster names, values are character vectors of node labels
      that must match matrix row/column names. Example:
      `list(Alpha = c("A", "B"), Beta = c("C", "D"))`

- method:

  Aggregation method for combining edge weights within/between clusters.
  Zero and `NA` weights are dropped before aggregation (see
  [`aggregate_weights`](https://sonsoles.me/cograph/reference/aggregate_weights.md)):

  "sum"

  :   (default) Sum of the edge weights. Suited to count data such as
      transition frequencies, because it preserves the total flow.

  "mean"

  :   Mean edge weight. Suited to inputs that are already transition
      probabilities, because the result does not grow with cluster size.

  "median"

  :   Median edge weight. Robust to outliers.

  "max"

  :   Maximum edge weight. Captures strongest connection.

  "min"

  :   Minimum edge weight. Captures weakest connection.

  "density"

  :   Sum divided by the number of possible edges (\\n_i n_j\\ for
      clusters of sizes \\n_i\\ and \\n_j\\).

  "geomean"

  :   Geometric mean of positive weights. Useful for multiplicative
      processes.

- type:

  Post-processing applied to aggregated weights. Determines the
  interpretation of the resulting matrices:

  "tna"

  :   (default) Row-normalize so each row sums to 1, which gives
      transition probabilities. Rows that sum to zero are left at zero.

  "raw"

  :   No normalization. The aggregated weights are returned as computed.

  "cooccurrence"

  :   Symmetrize the matrix as (A + t(A)) / 2.

  "semi_markov"

  :   Row-normalize, identical to `"tna"`.

- directed:

  Logical. Default `TRUE`. The value is recorded in `meta$directed` and
  does not change the weights. With `type = "cooccurrence"` the weights
  are symmetrized and `meta$directed` is `FALSE` whatever this value is.

- compute_within:

  Logical. If `TRUE` (default), compute per-cluster matrices, one \\n_i
  \times n_i\\ matrix of internal node-to-node weights per cluster.
  `FALSE` skips this step when only the macro summary is needed.

## Value

A `cluster_summary` object (S3 class) containing:

- macro:

  A tna object representing the macro (cluster-level) network:

  weights

  :   k x k matrix of cluster-to-cluster weights, where k is the number
      of clusters. Row i, column j contains the aggregated weight from
      cluster i to cluster j. The diagonal contains the aggregated
      within-cluster weight, node self-loops included. Processing
      depends on `type`.

  inits

  :   Named numeric vector of length k, the column sums of the
      aggregated cluster matrix (before `type` processing) divided by
      their total. It is uniform when all weights are zero.

  labels

  :   Cluster names.

  data

  :   Sequence data of a tna input, otherwise `NULL`.

- clusters:

  A `group_tna` list with one tna object per cluster, each containing:

  weights

  :   n_i x n_i matrix for nodes inside that cluster. Shows internal
      transitions between nodes in the same cluster.

  inits

  :   Column sums of the within-cluster weights divided by their total.

  NULL if `compute_within = FALSE`.

- cluster_members:

  Named list mapping cluster names to their member node labels. Example:
  `list(A = c("n1", "n2"), B = c("n3", "n4", "n5"))`

- meta:

  List of metadata:

  type

  :   The `type` argument used ("tna", "raw", etc.)

  method

  :   The `method` argument used ("sum", "mean", etc.)

  directed

  :   Logical, effective directedness of the stored weights (`FALSE`
      when `type = "cooccurrence"`, which symmetrizes them)

  n_nodes

  :   Total number of nodes in original network

  n_clusters

  :   Number of clusters

  cluster_sizes

  :   Named vector of cluster sizes

## Details

The function is the matrix-based entry point to Multi-Cluster
Multi-Level (MCML) analysis.
[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md) converts the
result to tna models.

### Workflow

A typical MCML analysis computes the summary, then plots it with
[`plot_mcml`](https://sonsoles.me/cograph/reference/plot_mcml.md) or
converts it to tna models with
[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md):


    cs <- csum(net, clusters = clusters, type = "tna")
    plot_mcml(cs)
    as_tna(cs)

### Between-Cluster Matrix Structure

The macro weight matrix has clusters as both rows and columns. An
off-diagonal cell (i, j) holds the aggregated weight from cluster i to
cluster j. A diagonal cell (i, i) holds the aggregated weight of the
edges inside cluster i. When `type = "tna"`, rows sum to 1 and the
diagonal is the probability of staying inside the same cluster.

### Choosing method and type

|  |  |  |
|----|----|----|
| Input data | Recommended | Reason |
| Edge counts | method="sum", type="tna" | Preserves total flow, normalizes to probabilities |
| Transition matrix | method="mean", type="tna" | Avoids cluster size bias |
| Frequencies | method="sum", type="raw" | Keep raw counts for analysis |
| Correlation matrix | method="mean", type="raw" | Average correlations |

## See also

[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md) to convert
results to tna objects,
[`plot_mcml`](https://sonsoles.me/cograph/reference/plot_mcml.md) for
two-layer visualization,
[`plot_mtna`](https://sonsoles.me/cograph/reference/plot_mtna.md) for
flat cluster visualization

## Examples

``` r
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
csum(regulation_net, clusters = clusters, type = "tna")
#> Cluster Summary
#> ---------------
#> Type: tna 
#> Method: sum 
#> Clusters: 3 
#> Nodes: 10 
#> Cluster sizes: 3, 3, 4 
#> 
#> Macro (cluster-level) weights (3x3):
#>   Inits: 0.339, 0.237, 0.423 
#>       C1    C2    C3
#> C1 0.652 0.255 0.093
#> C2 0.150 0.278 0.572
#> C3 0.404 0.175 0.422
#> 
#> Per-cluster weights:
#>   C1 (3 nodes)
#>   C2 (3 nodes)
#>   C3 (4 nodes)
```
