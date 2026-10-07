# Cluster Quality Metrics

Computes per-cluster and global quality metrics for network
partitioning. Supports both binary and weighted networks.

## Usage

``` r
cluster_quality(x, clusters, weighted = TRUE, directed = TRUE)

cqual(x, clusters, weighted = TRUE, directed = TRUE)
```

## Arguments

- x:

  Adjacency matrix (numeric)

- clusters:

  Cluster specification (named list, data frame, or membership vector;
  see [`csum`](https://sonsoles.me/cograph/reference/csum.md))

- weighted:

  Logical; if TRUE (default), use edge weights; if FALSE, binarize the
  matrix first

- directed:

  Logical; if TRUE (default), treat as directed network

## Value

A `cluster_quality` object (a list) with:

- per_cluster:

  Data frame, one row per cluster, with columns `cluster` (index),
  `cluster_name`, `n_nodes`, `internal_edges` (within-cluster weight),
  `cut_edges` (boundary-crossing weight), `internal_density`,
  `avg_internal_degree`, `expansion`, `cut_ratio` and `conductance`.

- global:

  List with `modularity` (Newman-Girvan, computed on the weighted or
  binarized matrix), `coverage` (share of total weight that is internal
  to some cluster) and `n_clusters`.

See `cluster_quality`.

## Examples

``` r
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
cluster_quality(regulation_net, clusters = clusters)
#> Cluster Quality Metrics
#> =======================
#> 
#> Global metrics:
#>   Modularity: 0.081 
#>   Coverage:   0.4033 
#>   Clusters:   3 
#> 
#> Per-cluster metrics:
#>  cluster cluster_name n_nodes internal_edges cut_edges internal_density
#>        1           C1       3           1.05      2.21       0.17500000
#>        2           C2       3           1.00      3.49       0.16666667
#>        3           C3       4           1.16      3.80       0.09666667
#>  avg_internal_degree expansion cut_ratio conductance
#>            0.7000000 0.7366667 0.1052381   0.5127610
#>            0.6666667 1.1633333 0.1661905   0.6357013
#>            0.5800000 0.9500000 0.1583333   0.6209150
```
