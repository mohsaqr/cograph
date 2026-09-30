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
mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
clusters <- c(1,1,1,2,2,2,3,3,3,3)

q <- cluster_quality(mat, clusters)
q$per_cluster   # Per-cluster metrics
#>   cluster cluster_name n_nodes internal_edges cut_edges internal_density
#> 1       1            1       3       3.312209  18.45718        0.5520348
#> 2       2            2       3       3.362291  18.80158        0.5603818
#> 3       3            3       4       5.066853  23.66512        0.4222378
#>   avg_internal_degree expansion cut_ratio conductance
#> 1            2.208139  6.152393 0.8789133   0.7358853
#> 2            2.241527  6.267192 0.8953131   0.7365611
#> 3            2.533427  5.916279 0.9860465   0.7001758
q$global        # Modularity, coverage
#> $modularity
#> [1] -0.06139144
#> 
#> $coverage
#> [1] 0.2782094
#> 
#> $n_clusters
#> [1] 3
#> 
mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
cqual(mat, c(1,1,1,2,2,2,3,3,3,3))
#> Cluster Quality Metrics
#> =======================
#> 
#> Global metrics:
#>   Modularity: -0.1103 
#>   Coverage:   0.229 
#>   Clusters:   3 
#> 
#> Per-cluster metrics:
#>  cluster cluster_name n_nodes internal_edges cut_edges internal_density
#>        1            1       3       3.363784  24.13739        0.5606306
#>        2            2       3       2.012768  24.71564        0.3354613
#>        3            3       4       5.943414  27.37954        0.4952845
#>  avg_internal_degree expansion cut_ratio conductance
#>             2.242522  8.045796  1.149399   0.7820322
#>             1.341845  8.238545  1.176935   0.8599383
#>             2.971707  6.844886  1.140814   0.6972772
```
