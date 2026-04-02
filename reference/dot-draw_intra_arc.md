# Draw a Smooth Bezier Arc for Intra-Group Edges

Uses a quadratic bezier curve with arc height proportional to distance,
ensuring consistent rounded arcs for both close and distant node pairs
(avoids the narrow spike issue with standard curvature for adjacent
nodes).

## Usage

``` r
.draw_intra_arc(
  x1,
  y1,
  x2,
  y2,
  intra_curvature,
  curve_sign,
  col,
  lwd,
  lty = 1,
  arrow,
  asize
)
```

## Arguments

- x1, y1:

  Start coordinates.

- x2, y2:

  End coordinates.

- intra_curvature:

  Base curvature amount (controls arc height ratio).

- curve_sign:

  Direction of curve (+1 or -1).

- col:

  Edge color.

- lwd:

  Line width.

- arrow:

  Draw arrow at target?

- asize:

  Arrow size.
