# Get Label Position on Edge

Calculates the position for an edge label (matches qgraph-style curves).
For curved edges, the label is offset perpendicular to the edge to avoid
overlapping with the edge line.

## Usage

``` r
get_edge_label_position(
  x1,
  y1,
  x2,
  y2,
  position = 0.5,
  curve = 0,
  curvePivot = 0.5,
  label_offset = 0
)
```

## Arguments

- x1, y1:

  Start point.

- x2, y2:

  End point.

- position:

  Position along edge (0-1).

- curve:

  Curvature amount.

- curvePivot:

  Curve pivot position.

- label_offset:

  Additional perpendicular offset for the label (in user coords).
  Positive values offset in the same direction as the curve bulge.
  Default 0.03 provides good separation from the edge line.

## Value

List with x, y coordinates.
