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

  List of adjacency matrices of the same dimensions

- method:

  Aggregation: "sum" (default), "mean", "max", "min", "union" or
  "intersection". `"union"` and `"intersection"` return a binary matrix
  of the cells with a positive weight in any or in every layer.

- weights:

  Optional numeric vector of layer weights, one per layer, used only by
  `method = "sum"` to compute a weighted sum.

## Value

The aggregated adjacency matrix, with the dimnames of the first layer. A
list with a single layer is returned unchanged for every `method`.

## Examples

``` r
layers <- list(forward = regulation_net, backward = t(regulation_net))
aggregate_layers(layers, method = "mean")
#>            Explore  Plan Monitor Adapt Reflect Discuss Synthesize Evaluate
#> Explore      0.000 0.000   0.000 0.140   0.200   0.150      0.000    0.000
#> Plan         0.000 0.000   0.065 0.000   0.000   0.200      0.055    0.245
#> Monitor      0.000 0.065   0.000 0.080   0.075   0.000      0.035    0.165
#> Adapt        0.140 0.000   0.080 0.000   0.000   0.170      0.085    0.215
#> Reflect      0.200 0.000   0.075 0.000   0.000   0.175      0.210    0.035
#> Discuss      0.150 0.200   0.000 0.170   0.175   0.000      0.000    0.000
#> Synthesize   0.000 0.055   0.035 0.085   0.210   0.000      0.000    0.000
#> Evaluate     0.000 0.245   0.165 0.215   0.035   0.000      0.000    0.000
#> Create       0.070 0.100   0.270 0.000   0.000   0.070      0.000    0.195
#> Share        0.135 0.285   0.245 0.195   0.000   0.000      0.000    0.000
#>            Create Share
#> Explore     0.070 0.135
#> Plan        0.100 0.285
#> Monitor     0.270 0.245
#> Adapt       0.000 0.195
#> Reflect     0.000 0.000
#> Discuss     0.070 0.000
#> Synthesize  0.000 0.000
#> Evaluate    0.195 0.000
#> Create      0.000 0.115
#> Share       0.115 0.000
```
