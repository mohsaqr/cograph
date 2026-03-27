# Compute Edge Curvatures

Determines per-edge curvature values based on reciprocal-edge detection,
curve mode, and layout geometry.

## Usage

``` r
compute_edge_curvatures(curvature, curves, edges, layout_mat)
```

## Arguments

- curvature:

  User-specified curvature scalar or vector.

- curves:

  Curve mode: FALSE, TRUE/"mutual", or "force".

- edges:

  Edge data frame with from/to columns.

- layout_mat:

  Two-column layout matrix.

## Value

List with `curves_vec`, `is_reciprocal`.
