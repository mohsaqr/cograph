# Draw Curved Edge with xspline (qgraph-style)

Renders a curved edge using xspline() with optional arrow. Uses
qgraph-style curve calculation for smooth, natural-looking curves. Curve
direction is normalized so positive curve always bends the same visual
direction regardless of edge orientation.

## Usage

``` r
draw_curved_edge_base(
  x1,
  y1,
  x2,
  y2,
  curve = 0.2,
  curvePivot = 0.5,
  col = "gray50",
  lwd = 1,
  lty = 1,
  arrow = TRUE,
  asize = 0.02,
  bidirectional = FALSE,
  start_lty = 1,
  start_fraction = 0,
  arrow_angle = pi/6
)
```

## Arguments

- x1, y1:

  Start point coordinates.

- x2, y2:

  End point coordinates.

- curve:

  Curvature amount (positive = clockwise, negative = counterclockwise
  when looking from source to target).

- curvePivot:

  Position along edge for control point (0-1).

- col:

  Edge color.

- lwd:

  Line width.

- lty:

  Line type.

- arrow:

  Logical: draw arrow at target?

- asize:

  Arrow size.

- bidirectional:

  Logical: draw arrow at source too?

- start_lty:

  Line type for start segment. 1=solid (default), 2=dashed, 3=dotted.

- start_fraction:

  Fraction of edge length for start segment (0-0.5). Default 0.

- arrow_angle:

  Arrow head angle in radians. Default pi/6 (30 degrees).
