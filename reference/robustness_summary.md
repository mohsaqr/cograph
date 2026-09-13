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

A data frame with one row per supplied (or computed) robustness result
and columns `measure`, `auc` (area under the robustness curve),
`critical_50` (fraction removed when the largest component first falls
below 50\\ same at 10\\ crossed. All numeric columns are rounded to 4
decimal places.

## Examples

``` r
g <- igraph::sample_pa(30, m = 2, directed = FALSE)
robustness_summary(x = g, measures = c("degree", "random"), n_iter = 10)
#>   measure    auc critical_50 critical_10
#> 1  degree 0.1922      0.1667      0.3667
#> 2  random 0.4459      0.4333      0.8000
```
