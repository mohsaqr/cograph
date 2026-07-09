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
nodes <- c("A", "B", "C")
l1 <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, dimnames = list(nodes, nodes))
l2 <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3, dimnames = list(nodes, nodes))
layers <- list(L1 = l1, L2 = l2)

aggregate_layers(layers, "sum")           # total edge weight
#>   A B C
#> A 0 2 1
#> B 2 0 1
#> C 1 1 0
aggregate_layers(layers, "mean")          # average edge weight
#>     A   B   C
#> A 0.0 1.0 0.5
#> B 1.0 0.0 0.5
#> C 0.5 0.5 0.0
aggregate_layers(layers, "union")         # edge present in any layer
#>   A B C
#> A 0 1 1
#> B 1 0 1
#> C 1 1 0
aggregate_layers(layers, "intersection")  # edge present in every layer
#>   A B C
#> A 0 1 0
#> B 1 0 0
#> C 0 0 0
```
