# Draw Self-Loop Edge (qgraph-style)

Renders a self-loop (edge from node to itself) using a teardrop/circular
loop shape similar to qgraph.

## Usage

``` r
draw_self_loop_base(
  x,
  y,
  node_size,
  col = "gray50",
  lwd = 1,
  lty = 1,
  rotation = pi/2,
  arrow = TRUE,
  asize = 0.02,
  arrow_angle = pi/6
)
```

## Arguments

- x, y:

  Node center coordinates.

- node_size:

  Node radius.

- col:

  Loop color.

- lwd:

  Line width.

- lty:

  Line type.

- rotation:

  Angle in radians for loop direction (default: pi/2 = top).

- arrow:

  Logical: draw arrow?

- asize:

  Arrow size.

- arrow_angle:

  Arrow head angle in radians. Default pi/6 (30 degrees).
