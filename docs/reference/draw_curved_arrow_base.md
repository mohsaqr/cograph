# Draw Curved Arrow Head

Draws an arrow head at the end of a curved edge, with angle following
the curve direction.

## Usage

``` r
draw_curved_arrow_base(
  spline_x,
  spline_y,
  size,
  arrow_angle = pi/6,
  col = "black",
  border = NULL
)
```

## Arguments

- spline_x:

  X coordinates of the spline.

- spline_y:

  Y coordinates of the spline.

- size:

  Arrow size.

- arrow_angle:

  Arrow head angle in radians. Default pi/6 (30 degrees).

- col:

  Arrow fill color.

- border:

  Arrow border color.
