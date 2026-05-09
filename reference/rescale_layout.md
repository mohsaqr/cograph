# Rescale Layout to -1 to 1 Range

Rescale Layout to -1 to 1 Range

## Usage

``` r
rescale_layout(layout, mar = 0.1, keep_aspect = TRUE)
```

## Arguments

- layout:

  Matrix or data frame with x, y columns.

- mar:

  Margin to leave (as proportion of range).

- keep_aspect:

  Logical. If TRUE, use one scale factor for both axes to preserve
  aspect ratio; if FALSE, scale axes independently.

## Value

Rescaled layout.
