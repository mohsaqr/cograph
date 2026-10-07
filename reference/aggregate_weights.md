# Aggregate Edge Weights

Aggregates a vector of edge weights into a single value. The method
names follow those of igraph's `edge.attr.comb` argument.

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
  `"geomean"` uses only the positive weights.

- n_possible:

  Number of possible edges (used only by `method = "density"`; when NULL
  or not positive, the number of surviving weights is used as the
  denominator instead).

## Value

A single numeric value, or 0 when no non-zero, non-NA weight remains.

## Examples

``` r
aggregate_weights(regulation_net, method = "mean")
#> [1] 0.2653333
```
