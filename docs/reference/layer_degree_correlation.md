# Degree Correlation Between Layers

Measures hub consistency across layers via degree correlation.

## Usage

``` r
layer_degree_correlation(layers, mode = c("total", "in", "out"))

ldegcor(layers, mode = c("total", "in", "out"))
```

## Arguments

- layers:

  List of adjacency matrices

- mode:

  Degree type: "total", "in", "out"

## Value

Correlation matrix between layer degree sequences

## Examples

``` r
mat1 <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
mat2 <- matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), 3, 3)
layers <- list(L1 = mat1, L2 = mat2)
layer_degree_correlation(layers, mode = "total")
#> Warning: the standard deviation is zero
#>    L1 L2
#> L1  1 NA
#> L2 NA  1
```
