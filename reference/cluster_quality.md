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
#> 1       1            1       3       3.786687  21.06550        0.6311144
#> 2       2            2       3       2.859344  21.89879        0.4765574
#> 3       3            3       4       6.569519  24.70585        0.5474599
#>   avg_internal_degree expansion cut_ratio conductance
#> 1            2.524458  7.021834  1.003119   0.7355562
#> 2            1.906229  7.299596  1.042799   0.7929323
#> 3            3.284760  6.176463  1.029411   0.6528187
q$global        # Modularity, coverage
#> $modularity
#> [1] -0.05841018
#> 
#> $coverage
#> [1] 0.2808794
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
#>   Modularity: -0.1139 
#>   Coverage:   0.2214 
#>   Clusters:   3 
#> 
#> Per-cluster metrics:
#>  cluster cluster_name n_nodes internal_edges cut_edges internal_density
#>        1            1       3       2.609745  23.94103        0.4349575
#>        2            2       3       3.044833  24.35574        0.5074722
#>        3            3       4       5.093286  27.31874        0.4244405
#>  avg_internal_degree expansion cut_ratio conductance
#>             1.739830  7.980343  1.140049   0.8210083
#>             2.029889  8.118581  1.159797   0.7999808
#>             2.546643  6.829684  1.138281   0.7283965
```
