# Cluster Summary Statistics

Aggregates node-level network weights to cluster-level summaries.
Computes both macro (cluster-to-cluster) transitions and per-cluster
transitions (how nodes connect inside each cluster).

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

  :   A cograph network object. The function extracts the weight matrix
      from `x$weights` or converts via
      [`to_matrix()`](https://sonsoles.me/cograph/reference/to_matrix.md).
      Clusters can be auto-detected from node attributes.

  tna

  :   A tna object from the tna package. Extracts `x$weights`.

  cluster_summary

  :   If already a cluster_summary, returns unchanged.

- clusters:

  Cluster/group assignments for nodes. Accepts multiple formats:

  NULL

  :   (default) Auto-detect from cograph_network. Looks for columns
      named 'clusters', 'cluster', 'groups', or 'group' in `x$nodes`.
      Throws an error if no cluster column is found. This option only
      works when `x` is a cograph_network.

  vector

  :   Cluster membership for each node, in the same order as the matrix
      rows/columns. Can be numeric (1, 2, 3) or character ("A", "B").
      Cluster names will be derived from unique values. Example:
      `c(1, 1, 2, 2, 3, 3)` assigns first two nodes to cluster 1.

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
  Controls how multiple node-to-node edges are summarized:

  "sum"

  :   (default) Sum of all edge weights. Best for count data (e.g.,
      transition frequencies). Preserves total flow.

  "mean"

  :   Average edge weight. Best when cluster sizes differ and you want
      to control for size. Note: when input is already a transition
      matrix (rows sum to 1), "mean" avoids size bias. Example: cluster
      with 5 nodes won't have 5x the weight of cluster with 1 node.

  "median"

  :   Median edge weight. Robust to outliers.

  "max"

  :   Maximum edge weight. Captures strongest connection.

  "min"

  :   Minimum edge weight. Captures weakest connection.

  "density"

  :   Sum divided by number of possible edges. Normalizes by cluster
      size combinations.

  "geomean"

  :   Geometric mean of positive weights. Useful for multiplicative
      processes.

- type:

  Post-processing applied to aggregated weights. Determines the
  interpretation of the resulting matrices:

  "tna"

  :   (default) Row-normalize so each row sums to 1. Creates transition
      probabilities suitable for Markov chain analysis. Interpretation:
      "Given I'm in cluster A, what's the probability of transitioning
      to cluster B?" Required for use with tna package functions.
      Diagonal is zero; per-cluster data is in `$clusters`.

  "raw"

  :   No normalization. Returns aggregated counts/weights as-is. Use for
      frequency analysis or when you need raw counts. Compatible with
      igraph's contract + simplify output.

  "cooccurrence"

  :   Symmetrize the matrix: (A + t(A)) / 2. For undirected
      co-occurrence analysis.

  "semi_markov"

  :   Row-normalize with duration weighting. For semi-Markov process
      analysis.

- directed:

  Logical. If `TRUE` (default), treat network as directed. A-\>B and
  B-\>A are separate edges. If `FALSE`, edges are undirected and the
  matrix is symmetrized before processing.

- compute_within:

  Logical. If `TRUE` (default), compute per-cluster transition matrices
  for each cluster. Each cluster gets its own n_i x n_i matrix showing
  internal node-to-node transitions. Set to `FALSE` to skip this
  computation for better performance when only the macro (cluster-level)
  summary is needed.

## Value

A `cluster_summary` object (S3 class) containing:

- macro:

  A tna object representing the macro (cluster-level) network:

  weights

  :   k x k matrix of cluster-to-cluster weights, where k is the number
      of clusters. Row i, column j contains the aggregated weight from
      cluster i to cluster j. Diagonal contains aggregated intra-cluster
      weight (retention / self-loops). Processing depends on `type`.

  inits

  :   Numeric vector of length k. Initial state distribution across
      clusters, computed from column sums of the original matrix.
      Represents the proportion of incoming edges to each cluster.

- clusters:

  Named list with one element per cluster. Each element is a tna object
  containing:

  weights

  :   n_i x n_i matrix for nodes inside that cluster. Shows internal
      transitions between nodes in the same cluster.

  inits

  :   Initial distribution for the cluster.

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

This is the core function for Multi-Cluster Multi-Level (MCML) analysis.
Use [`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md) to
convert results to tna objects for further analysis with the tna
package.

### Workflow

Typical MCML analysis workflow:


    # 1. Create network
    net <- cograph(edges, nodes = nodes)
    net$nodes$clusters <- group_assignments

    # 2. Compute cluster summary
    cs <- csum(net, type = "tna")

    # 3. Convert to tna models
    tna_models <- as_tna(cs)

    # 4. Analyze/visualize
    plot(tna_models$macro)
    tna::centralities(tna_models$macro)

### Between-Cluster Matrix Structure

The `macro$weights` matrix has clusters as both rows and columns:

- Off-diagonal (row i, col j): Aggregated weight from cluster i to
  cluster j

- Diagonal (row i, col i): Per-cluster total (sum of internal edges in
  cluster i)

When `type = "tna"`, rows sum to 1 and diagonal values represent
"retention rate" - the probability of staying inside the same cluster.

### Choosing method and type

|  |  |  |
|----|----|----|
| **Input data** | **Recommended** | **Reason** |
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
mat <- matrix(runif(100), 10, 10); diag(mat) <- 0
rownames(mat) <- colnames(mat) <- LETTERS[1:10]

# Membership vector
cs <- csum(mat, c(1,1,1,2,2,2,3,3,3,3))
cs$macro$weights      # 3x3 cluster transition matrix
#>           1         2         3
#> 1 0.1838662 0.1963128 0.6198211
#> 2 0.4730634 0.2032878 0.3236488
#> 3 0.4026309 0.2624785 0.3348906

# Named list of clusters, TNA-normalized
clusters <- list(Alpha = LETTERS[1:3], Beta = LETTERS[4:6], Gamma = LETTERS[7:10])
cs <- csum(mat, clusters, type = "tna")
rowSums(cs$macro$weights)  # all 1 (TNA probabilities)
#> Alpha  Beta Gamma 
#>     1     1     1 
```
