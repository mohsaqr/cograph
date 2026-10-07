# Calculate Area Under Robustness Curve (AUC)

Computes the area under the robustness curve (`comp_pct` against
`removed_pct`) by trapezoidal integration. A higher AUC indicates a more
robust network. The maximum AUC is 1.

## Usage

``` r
robustness_auc(x)
```

## Arguments

- x:

  A robustness result from
  [`robustness`](https://sonsoles.me/cograph/reference/robustness.md),
  or any data frame with columns `removed_pct` and `comp_pct`. Other
  input raises an error.

## Value

A single numeric AUC value between 0 and 1.

## Examples

``` r
robustness_auc(robustness(regulation_net, measure = "degree"))
#> [1] 0.43
```
