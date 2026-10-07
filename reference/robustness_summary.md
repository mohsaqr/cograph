# Summary of Robustness Analysis

Summarizes robustness results across attack strategies.

## Usage

``` r
robustness_summary(..., x = NULL, measures = NULL, n_iter = 1000)
```

## Arguments

- ...:

  Robustness results from
  [`robustness`](https://sonsoles.me/cograph/reference/robustness.md).
  When these are supplied, `x` is ignored.

- x:

  Network on which vertex robustness is computed for each of `measures`.
  Used when `...` is empty.

- measures:

  Measures to compute when `x` is supplied. Default NULL uses
  c("betweenness", "degree", "random").

- n_iter:

  Iterations for random removal. Default 1000.

## Value

A data frame with one row per supplied (or computed) robustness result
and columns `measure`, `auc` (area under the robustness curve),
`critical_50` (fraction removed when the largest component first falls
below 50\\ same at 10\\ crossed. All numeric columns are rounded to 4
decimal places. Supplying neither results nor `x` raises an error.

## Examples

``` r
robustness_summary(x = regulation_net, measures = c("degree", "random"), n_iter = 10)
#>   measure   auc critical_50 critical_10
#> 1  degree 0.430         0.5           1
#> 2  random 0.498         0.6           1
```
