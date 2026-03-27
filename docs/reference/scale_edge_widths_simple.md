# Scale Edge Widths (Simple Version)

Simple linear edge width scaling used by sn_edges() when width="weight".
For the full-featured version with multiple modes and cut parameter, see
scale_edge_widths() in scale-constants.R.

## Usage

``` r
scale_edge_widths_simple(values, range = c(0.5, 3), maximum = NULL)
```

## Arguments

- values:

  Numeric values to scale.

- range:

  Target width range (min, max).

- maximum:

  Optional maximum value for scaling. If provided, this value maps to
  the max of range, and values above it are capped.

## Value

Scaled width values.
