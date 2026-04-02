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
if (FALSE) { # \dontrun{
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_pa(30, m = 2, directed = FALSE)
  robustness_summary(x = g, measures = c("degree", "random"), n_iter = 10)
}
} # }
```
