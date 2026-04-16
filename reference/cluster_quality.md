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

  Adjacency matrix

- clusters:

  Cluster specification (list or membership vector)

- weighted:

  Logical; if TRUE, use edge weights; if FALSE, binarize

- directed:

  Logical; if TRUE, treat as directed network

## Value

A `cluster_quality` object with:

- per_cluster:

  Data frame with per-cluster metrics

- global:

  List of global metrics (modularity, coverage)

See `cluster_quality`.

## Examples

``` r
mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
clusters <- c(1,1,1,2,2,2,3,3,3,3)

q <- cluster_quality(mat, clusters)
q$per_cluster   # Per-cluster metrics
#>   cluster cluster_name n_nodes internal_edges cut_edges internal_density
#> 1       1            1       3       2.937610  20.93125        0.4896017
#> 2       2            2       3       3.206718  22.67670        0.5344529
#> 3       3            3       4       6.574337  25.20272        0.5478615
#>   avg_internal_degree expansion cut_ratio conductance
#> 1            1.958407  6.977083 0.9967262   0.7808283
#> 2            2.137812  7.558900 1.0798429   0.7795323
#> 3            3.287169  6.300679 1.0501132   0.6571526
q$global        # Modularity, coverage
#> $modularity
#> [1] -0.07088779
#> 
#> $coverage
#> [1] 0.2698978
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
#>   Modularity: -0.0916 
#>   Coverage:   0.2489 
#>   Clusters:   3 
#> 
#> Per-cluster metrics:
#>  cluster cluster_name n_nodes internal_edges cut_edges internal_density
#>        1            1       3       3.573873  22.84317        0.5956454
#>        2            2       3       2.872407  23.35599        0.4787344
#>        3            3       4       5.932966  28.49671        0.4944138
#>  avg_internal_degree expansion cut_ratio conductance
#>             2.382582  7.614389  1.087770   0.7616696
#>             1.914938  7.785328  1.112190   0.8025892
#>             2.966483  7.124178  1.187363   0.7060170
```
