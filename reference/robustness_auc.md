# Calculate Area Under Robustness Curve (AUC)

Computes the area under the robustness curve using trapezoidal
integration. Higher AUC indicates a more robust network. Maximum AUC is
1.0.

## Usage

``` r
robustness_auc(x)
```

## Arguments

- x:

  A robustness result from
  [`robustness`](http://sonsoles.me/cograph/reference/robustness.md).

## Value

Numeric AUC value between 0 and 1.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_pa(30, m = 2, directed = FALSE)

  rob_btw <- robustness(g, measure = "betweenness")
  rob_rnd <- robustness(g, measure = "random", n_iter = 20)

  cat("Betweenness attack AUC:", round(robustness_auc(rob_btw), 3), "\n")
  cat("Random failure AUC:", round(robustness_auc(rob_rnd), 3), "\n")
}
#> Betweenness attack AUC: 0.196 
#> Random failure AUC: 0.434 
```
