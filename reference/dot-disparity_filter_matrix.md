# Disparity Filter Core Implementation

Exact implementation matching TNA package. Uses fast .rowSums/.colSums
for performance.

## Usage

``` r
.disparity_filter_matrix(mat, level = 0.05)
```

## Arguments

- mat:

  Weight matrix.

- level:

  Significance level.

## Value

Binary significance matrix.
