# Aggregate Layers

Combines multiple network layers into a single network.

## Usage

``` r
aggregate_layers(
  layers,
  method = c("sum", "mean", "max", "min", "union", "intersection"),
  weights = NULL
)

lagg(
  layers,
  method = c("sum", "mean", "max", "min", "union", "intersection"),
  weights = NULL
)
```

## Arguments

- layers:

  List of adjacency matrices

- method:

  Aggregation: "sum", "mean", "max", "min", "union", "intersection"

- weights:

  Optional layer weights (for weighted sum)

## Value

Aggregated adjacency matrix

## Examples

``` r
# layers <- list(L1 = mat1, L2 = mat2, L3 = mat3)
# aggregate_layers(layers, "sum")           # Total
# aggregate_layers(layers, "mean")          # Average
# aggregate_layers(layers, "union")         # Any edge
# aggregate_layers(layers, "intersection")  # All edges
```
