# Build MCML from Raw Transition Data

Builds a Multi-Cluster Multi-Level (MCML) model from raw transition data
(edge lists or sequences) by recoding node labels to cluster labels and
counting the observed transitions. The macro network is then the Markov
chain over cluster states. Weight matrices are passed to
[`csum`](https://sonsoles.me/cograph/reference/csum.md), which
aggregates them.

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

  group_tna

  :   Converted as by
      [`as_mcml`](https://sonsoles.me/cograph/reference/as_mcml.md).

  mcml or cluster_summary

  :   Returned unchanged.

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
  normalization), "cooccurrence" (symmetrize), or "semi_markov"
  (row-normalize, identical to "tna"). Default "tna".

- directed:

  Logical. Default `TRUE`. The value is recorded in `meta$directed`; the
  weights are not modified.

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
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
summarize_clusters(regulation_net, clusters = clusters, method = "mean")
#> MCML Network
#> ============
#> Type: tna  | Method: mean 
#> Nodes: 10  | Clusters: 3 
#> 
#> Clusters:
#>   C1 (3): Explore, Reflect, Discuss
#>   C2 (3): Plan, Create, Share
#>   C3 (4): Monitor, Adapt, Synthesize, Evaluate
#> 
#> Macro (cluster-level) weights:
#>        C1     C2     C3
#> C1 0.4251 0.3320 0.2429
#> C2 0.3127 0.2896 0.3977
#> C3 0.3702 0.3202 0.3095
```
