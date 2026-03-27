# Summary of Robustness Analysis

Provides a summary comparing robustness metrics across attack
strategies.

## Usage

``` r
robustness_summary(..., x = NULL, measures = NULL, n_iter = 1000)
```

## Arguments

- ...:

  Robustness results to summarize.

- x:

  Network for on-the-fly computation.

- measures:

  Measures to compute if x provided.

- n_iter:

  Iterations for random. Default 1000.

## Value

Data frame with AUC and critical points for each measure.

## Examples

``` r
# \donttest{
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_pa(30, m = 2, directed = FALSE)
  robustness_summary(x = g, measures = c("degree", "random"), n_iter = 10)
}
#>   measure    auc critical_50 critical_10
#> 1  degree 0.1911      0.1667      0.3333
#> 2  random 0.4253      0.4333      0.8000
# }
```
