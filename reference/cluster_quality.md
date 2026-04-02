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
#> 1       1            1       3       2.043959  21.44976        0.3406598
#> 2       2            2       3       3.016079  23.13639        0.5026799
#> 3       3            3       4       9.159230  23.36489        0.7632691
#>   avg_internal_degree expansion cut_ratio conductance
#> 1            1.362639  7.149920 1.0214171   0.8399260
#> 2            2.010720  7.712130 1.1017328   0.7931965
#> 3            4.579615  5.841223 0.9735371   0.5605329
q$global        # Modularity, coverage
#> $modularity
#> [1] -0.05272447
#> 
#> $coverage
#> [1] 0.2950375
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
#>   Modularity: -0.0673 
#>   Coverage:   0.271 
#>   Clusters:   3 
#> 
#> Per-cluster metrics:
#>  cluster cluster_name n_nodes internal_edges cut_edges internal_density
#>        1            1       3       4.392079  21.96938        0.7320132
#>        2            2       3       2.757767  22.01327        0.4596278
#>        3            3       4       5.831567  25.85041        0.4859639
#>  avg_internal_degree expansion cut_ratio conductance
#>             2.928053  7.323126  1.046161   0.7143691
#>             1.838511  7.337758  1.048251   0.7996450
#>             2.915783  6.462602  1.077100   0.6890954
```
