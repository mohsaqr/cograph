# Deprecated Alias for csum

`mcml()` is deprecated and kept for backward compatibility. It calls
[`csum`](https://sonsoles.me/cograph/reference/csum.md) with
`type = "tna"`, and new code uses
[`csum()`](https://sonsoles.me/cograph/reference/csum.md) directly.

## Usage

``` r
mcml(
  x,
  cluster_list = NULL,
  aggregation = c("sum", "mean", "max"),
  as_tna = FALSE,
  nodes = NULL,
  within = TRUE
)
```

## Arguments

- x:

  Weight matrix, tna object, cograph_network, or cluster_summary object.

- cluster_list:

  Named list of node vectors per cluster.

- aggregation:

  How edge weights are aggregated, one of `"sum"`, `"mean"`, `"max"`.

- as_tna:

  Logical. If `TRUE`, a tna-compatible object is returned.

- nodes:

  Node metadata data frame, stored with the result for display labels.

- within:

  Logical. Whether within-cluster matrices are computed.

## Value

A `cluster_summary` object, or a tna object if `as_tna = TRUE`.

## Examples

``` r
mcml(regulation_net, cluster_list = list(
  Plan = c("Explore", "Plan", "Monitor", "Adapt", "Reflect"),
  Act = c("Discuss", "Synthesize", "Evaluate", "Create", "Share")
))
#> Cluster Summary
#> ---------------
#> Type: tna 
#> Method: sum 
#> Clusters: 2 
#> Nodes: 10 
#> Cluster sizes: 5, 5 
#> 
#> Macro (cluster-level) weights (2x2):
#>   Inits: 0.578, 0.422 
#>       Plan   Act
#> Plan 0.301 0.699
#> Act  0.821 0.179
#> 
#> Per-cluster weights:
#>   Plan (5 nodes)
#>   Act (5 nodes)
```
