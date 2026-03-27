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

  Numeric vector of edge weights

- method:

  Aggregation method: "sum", "mean", "median", "max", "min", "prod",
  "density", "geomean"

- n_possible:

  Number of possible edges (for density calculation)

## Value

Single aggregated value

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
