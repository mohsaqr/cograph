# Find Split Index for Curve Based on Arc Length Fraction

Calculates the index at which to split a curve's coordinate arrays so
that the first segment covers a given fraction of the total arc length.

## Usage

``` r
find_curve_split_index(x, y, fraction)
```

## Arguments

- x, y:

  Vectors of curve coordinates.

- fraction:

  Desired fraction of total arc length (0-1).

## Value

Index at which to split the arrays.
