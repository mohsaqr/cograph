# Degree Correlation Between Layers

Measures the consistency of hubs across layers as the Pearson
correlation of node degrees between layers.

## Usage

``` r
layer_degree_correlation(layers, mode = c("total", "in", "out"))

ldegcor(layers, mode = c("total", "in", "out"))
```

## Arguments

- layers:

  List of adjacency matrices of the same dimensions

- mode:

  Degree type: "total" (default, row plus column sums), "in" (column
  sums) or "out" (row sums). The sums use the edge weights, so on a
  weighted layer the degree is the node strength.

## Value

An L x L Pearson correlation matrix of the layer degree sequences, with
the layer names (or `"Layer1"`, `"Layer2"`, ...) as dimnames.

## Examples

``` r
layers <- list(forward = regulation_net, backward = t(regulation_net))
layer_degree_correlation(layers, mode = "total")
#>          forward backward
#> forward        1        1
#> backward       1        1
```
