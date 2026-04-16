# mcml - Deprecated alias for cluster_summary

**\[deprecated\]**

Use
[`cluster_summary`](https://sonsoles.me/cograph/reference/cluster_summary.md)
instead. This function is provided for backward compatibility only.

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

  Weight matrix, tna object, cograph_network, or cluster_summary object

- cluster_list:

  Named list of node vectors per cluster

- aggregation:

  How to aggregate edge weights: "sum", "mean", "max"

- as_tna:

  Logical. If TRUE, return a tna-compatible object

- nodes:

  Node metadata

- within:

  Logical. Compute within-cluster matrices

## Value

A cluster_summary object (or tna if as_tna = TRUE)

## Examples

``` r
if (FALSE) { # \dontrun{
mat <- matrix(runif(100, 0, 0.3), 10, 10)
diag(mat) <- 0
colnames(mat) <- rownames(mat) <- paste0("N", 1:10)
clusters <- list(C1 = paste0("N", 1:5), C2 = paste0("N", 6:10))
mcml(mat, clusters)
} # }
```
