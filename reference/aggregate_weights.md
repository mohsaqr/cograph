# Aggregate Edge Weights

Aggregates a vector of edge weights using various methods. Compatible
with igraph's edge.attr.comb parameter.

## Usage

``` r
aggregate_weights(w, method = "sum", n_possible = NULL)

wagg(w, method = "sum", n_possible = NULL)
```

## Arguments

- w:

  Numeric vector of edge weights. `NA` and zero entries are dropped
  before aggregation.

- method:

  Aggregation method: "sum", "mean", "median", "max", "min", "prod",
  "density", "geomean". Default "sum". Any other value is an error.

- n_possible:

  Number of possible edges (used only by `method = "density"`; when NULL
  or not positive, the number of surviving weights is used as the
  denominator instead).

## Value

A single numeric value, or 0 when no non-zero, non-NA weight remains.

## Examples

``` r
w <- c(0.5, 0.8, 0.3, 0.9)
aggregate_weights(w, "sum")   # 2.5
#> [1] 2.5
aggregate_weights(w, "mean")  # 0.625
#> [1] 0.625
aggregate_weights(w, "max")   # 0.9
#> [1] 0.9
```
