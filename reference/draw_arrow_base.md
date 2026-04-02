# Draw Arrow Head

Draws a filled triangular arrow head at the specified position.

## Usage

``` r
draw_arrow_base(
  x,
  y,
  angle,
  size,
  arrow_angle = pi/6,
  col = "black",
  border = NULL,
  lwd = 1
)
```

## Arguments

- x:

  Arrow tip x coordinate.

- y:

  Arrow tip y coordinate.

- angle:

  Angle of incoming edge (radians).

- size:

  Arrow size in user coordinates.

- arrow_angle:

  Arrow head angle in radians. Default pi/6 (30 degrees).

- col:

  Arrow fill color.

- border:

  Arrow border color (default same as fill).

- lwd:

  Border line width.
