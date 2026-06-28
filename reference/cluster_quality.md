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
#> 1       1            1       3       3.206718  22.79075        0.5344529
#> 2       2            2       3       2.908608  23.51103        0.4847681
#> 3       3            3       4       6.730487  27.18351        0.5608740
#>   avg_internal_degree expansion cut_ratio conductance
#> 1            2.137812  7.596917  1.085274   0.7803933
#> 2            1.939072  7.837010  1.119573   0.8016514
#> 3            3.365244  6.795878  1.132646   0.6688118
q$global        # Modularity, coverage
#> $modularity
#> [1] -0.08264454
#> 
#> $coverage
#> [1] 0.2590484
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
#>   Modularity: -0.085 
#>   Coverage:   0.2525 
#>   Clusters:   3 
#> 
#> Per-cluster metrics:
#>  cluster cluster_name n_nodes internal_edges cut_edges internal_density
#>        1            1       3       2.872407  22.87844        0.4787344
#>        2            2       3       2.863043  24.22716        0.4771738
#>        3            3       4       6.405012  24.76275        0.5337510
#>  avg_internal_degree expansion cut_ratio conductance
#>             1.914938  7.626147  1.089450   0.7992956
#>             1.908695  8.075719  1.153674   0.8088325
#>             3.202506  6.190687  1.031781   0.6590610
```
