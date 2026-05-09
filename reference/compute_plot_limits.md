# Compute Plot Limits

Calculates xlim/ylim accounting for node radii, self-loop extents, and
layout margin padding.

## Usage

``` r
compute_plot_limits(
  layout_mat,
  vsize_usr,
  layout_margin,
  edges,
  n_edges,
  loop_rotations,
  fixed_bounds = NULL
)
```

## Arguments

- layout_mat:

  Two-column layout matrix.

- vsize_usr:

  Node radii in user coordinates.

- layout_margin:

  Fractional margin padding.

- edges:

  Edge data frame.

- n_edges:

  Number of edges.

- loop_rotations:

  Per-edge loop rotation angles.

- fixed_bounds:

  Optional numeric vector `c(xmin, xmax, ymin, ymax)` for fixed plot
  bounds before node/loop padding.

## Value

List with `xlim` and `ylim`.
