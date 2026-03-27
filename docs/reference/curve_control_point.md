# Calculate Control Point for Curved Edge

Calculate Control Point for Curved Edge

## Usage

``` r
curve_control_point(x1, y1, x2, y2, curvature, pivot = 0.5, shape = 0)
```

## Arguments

- x1, y1:

  Start point.

- x2, y2:

  End point.

- curvature:

  Curvature amount (0 = straight line).

- pivot:

  Position along edge (0-1) where control point sits. 0 = near source,
  0.5 = middle (default), 1 = near target.

- shape:

  Spline tension affecting curvature intensity (-1 to 1). Negative =
  sharper curve, Positive = gentler curve. Default 0.

## Value

List with x, y coordinates of control point.
