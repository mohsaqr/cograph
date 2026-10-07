# Convert to mcml

Converts an object to the `mcml` class, a representation of a
multi-cluster network that does not depend on the tna package.

## Usage

``` r
as_mcml(x, ...)

# S3 method for class 'cluster_summary'
as_mcml(x, ...)

# S3 method for class 'group_tna'
as_mcml(x, clusters = NULL, method = "sum", type = "tna", directed = TRUE, ...)

# S3 method for class 'mcml'
as_mcml(x, ...)

# Default S3 method
as_mcml(x, ...)
```

## Arguments

- x:

  A `cluster_summary`, `group_tna` or `mcml` object. Any other input is
  an error;
  [`summarize_clusters`](https://sonsoles.me/cograph/reference/summarize_clusters.md)
  builds an `mcml` object from raw data.

- ...:

  Additional arguments passed to methods.

- clusters:

  Integer or character vector of row-to-group assignments. Required when
  the `group_tna` has the same labels across all groups (row-level
  clustering from `tna::group_model(cluster_data(...))`).

- method:

  Aggregation method for macro weights (default `"sum"`).

- type:

  Transition type (default `"tna"`).

- directed:

  Logical; whether the network is directed (default `TRUE`).

## Value

An `mcml` object with components `macro`, `clusters`, `cluster_members`,
and `meta`.

An `mcml` object.

An `mcml` object. When `clusters` is provided, `macro$data` contains the
cluster assignments and `macro$weights` is `NULL`.

The input `mcml` object unchanged.

## See also

[`summarize_clusters`](https://sonsoles.me/cograph/reference/summarize_clusters.md),
[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md)

## Examples

``` r
clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                 C2 = c("Plan", "Create", "Share"),
                 C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
as_mcml(csum(regulation_net, clusters = clusters, type = "tna"))
#> $macro
#> $macro$weights
#>           C1        C2        C3
#> C1 0.6521739 0.2546584 0.0931677
#> C2 0.1500000 0.2777778 0.5722222
#> C3 0.4036364 0.1745455 0.4218182
#> 
#> $macro$inits
#>        C1        C2        C3 
#> 0.3391960 0.2374372 0.4233668 
#> 
#> $macro$labels
#> [1] "C1" "C2" "C3"
#> 
#> $macro$data
#> NULL
#> 
#> 
#> $clusters
#> $clusters$C1
#> $clusters$C1$weights
#>           Explore   Reflect Discuss
#> Explore 0.0000000 1.0000000       0
#> Reflect 1.0000000 0.0000000       0
#> Discuss 0.4615385 0.5384615       0
#> 
#> $clusters$C1$inits
#>   Explore   Reflect   Discuss 
#> 0.3333333 0.6666667 0.0000000 
#> 
#> $clusters$C1$labels
#> [1] "Explore" "Reflect" "Discuss"
#> 
#> $clusters$C1$data
#> NULL
#> 
#> 
#> $clusters$C2
#> $clusters$C2$weights
#>        Plan    Create     Share
#> Plan      0 0.3571429 0.6428571
#> Create    0 0.0000000 1.0000000
#> Share     1 0.0000000 0.0000000
#> 
#> $clusters$C2$inits
#>   Plan Create  Share 
#>   0.21   0.20   0.59 
#> 
#> $clusters$C2$labels
#> [1] "Plan"   "Create" "Share" 
#> 
#> $clusters$C2$data
#> NULL
#> 
#> 
#> $clusters$C3
#> $clusters$C3$weights
#>              Monitor     Adapt Synthesize Evaluate
#> Monitor    0.0000000 1.0000000          0        0
#> Adapt      0.0000000 0.0000000          1        0
#> Synthesize 1.0000000 0.0000000          0        0
#> Evaluate   0.4342105 0.5657895          0        0
#> 
#> $clusters$C3$inits
#>    Monitor      Adapt Synthesize   Evaluate 
#>  0.3448276  0.5086207  0.1465517  0.0000000 
#> 
#> $clusters$C3$labels
#> [1] "Monitor"    "Adapt"      "Synthesize" "Evaluate"  
#> 
#> $clusters$C3$data
#> NULL
#> 
#> 
#> 
#> $cluster_members
#> $cluster_members$C1
#> [1] "Explore" "Reflect" "Discuss"
#> 
#> $cluster_members$C2
#> [1] "Plan"   "Create" "Share" 
#> 
#> $cluster_members$C3
#> [1] "Monitor"    "Adapt"      "Synthesize" "Evaluate"  
#> 
#> 
#> $meta
#> $meta$type
#> [1] "tna"
#> 
#> $meta$method
#> [1] "sum"
#> 
#> $meta$directed
#> [1] TRUE
#> 
#> $meta$n_nodes
#> [1] 10
#> 
#> $meta$n_clusters
#> [1] 3
#> 
#> $meta$cluster_sizes
#> C1 C2 C3 
#>  3  3  4 
#> 
#> 
#> attr(,"class")
#> [1] "mcml"
```
