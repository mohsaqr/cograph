# Build MCML from Raw Transition Data

Builds a Multi-Cluster Multi-Level (MCML) model from raw transition data
(edge lists or sequences) by recoding node labels to cluster labels and
counting actual transitions. Unlike
[`csum`](https://sonsoles.me/cograph/reference/csum.md) which aggregates
a pre-computed weight matrix, this function works from the original
transition data to produce the TRUE Markov chain over cluster states.

## Usage

``` r
summarize_clusters(
  x,
  clusters = NULL,
  method = c("sum", "mean", "median", "max", "min", "density", "geomean"),
  type = c("tna", "frequency", "cooccurrence", "semi_markov", "raw"),
  directed = TRUE,
  compute_within = TRUE
)
```

## Arguments

- x:

  Input data. Accepts multiple formats:

  data.frame with from/to columns

  :   Edge list. Columns named from/source/src/v1/node1/i and
      to/target/tgt/v2/node2/j are auto-detected. Optional weight column
      (weight/w/value/strength).

  data.frame without from/to columns

  :   Sequence data. Each row is a sequence, columns are time steps.
      Consecutive pairs (t, t+1) become transitions.

  tna object

  :   If `x$data` is non-NULL, uses sequence path on the raw data.
      Otherwise falls back to
      [`csum`](https://sonsoles.me/cograph/reference/csum.md).

  cograph_network

  :   If `x$data` is non-NULL, detects edge list vs sequence data.
      Otherwise falls back to
      [`csum`](https://sonsoles.me/cograph/reference/csum.md).

  cluster_summary

  :   Returns as-is.

  square numeric matrix

  :   Falls back to
      [`csum`](https://sonsoles.me/cograph/reference/csum.md).

  non-square or character matrix

  :   Treated as sequence data.

- clusters:

  Cluster/group assignments. Accepts:

  named list

  :   Direct mapping. List names = cluster names, values = character
      vectors of node labels. Example:
      `list(A = c("N1","N2"), B = c("N3","N4"))`

  data.frame

  :   A data frame where the first column contains node names and the
      second column contains group/cluster names. Example:
      `data.frame(node = c("N1","N2","N3"), group = c("A","A","B"))`

  membership vector

  :   Character or numeric vector. Node names are extracted from the
      data. Example: `c("A","A","B","B")`

  column name string

  :   For edge list data.frames, the name of a column containing cluster
      labels. The mapping is built from unique (node, group) pairs in
      both from and to columns.

  NULL

  :   Auto-detect from `cograph_network$nodes` or `$node_groups` (same
      logic as [`csum`](https://sonsoles.me/cograph/reference/csum.md)).

- method:

  Aggregation method for combining edge weights: "sum", "mean",
  "median", "max", "min", "density", "geomean". Default "sum".

- type:

  Post-processing: "tna" (row-normalize), "frequency" or "raw" (no
  normalization), "cooccurrence" (symmetrize), or "semi_markov". Default
  "tna".

- directed:

  Logical. Treat as directed network? Default TRUE.

- compute_within:

  Logical. Compute within-cluster matrices? Default TRUE.

## Value

Usually an `mcml` object. Existing `mcml` or `cluster_summary` inputs
are returned unchanged. Transition-data results include
`meta$source = "transitions"` and are compatible with
[`plot_mcml`](https://sonsoles.me/cograph/reference/plot_mcml.md),
[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md), and
[`splot`](https://sonsoles.me/cograph/reference/splot.md).

## See also

[`csum`](https://sonsoles.me/cograph/reference/csum.md) for matrix-based
aggregation, [`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md)
to convert to tna objects,
[`plot_mcml`](https://sonsoles.me/cograph/reference/plot_mcml.md) for
visualization

## Examples

``` r
# Edge list with clusters
edges <- data.frame(
  from = c("A", "A", "B", "C", "C", "D"),
  to   = c("B", "C", "A", "D", "D", "A"),
  weight = c(1, 2, 1, 3, 1, 2)
)
clusters <- list(G1 = c("A", "B"), G2 = c("C", "D"))
cs <- summarize_clusters(edges, clusters)
cs$macro$weights
#>           G1        G2
#> G1 0.5000000 0.5000000
#> G2 0.3333333 0.6666667

# Sequence data with clusters
seqs <- data.frame(
  T1 = c("A", "C", "B"),
  T2 = c("B", "D", "A"),
  T3 = c("C", "C", "D"),
  T4 = c("D", "A", "C")
)
cs <- summarize_clusters(seqs, clusters, type = "raw")
cs$macro$weights
#>    G1 G2
#> G1  2  2
#> G2  1  4
```
