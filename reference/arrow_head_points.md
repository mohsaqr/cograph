# Calculate Arrow Head Points

Returns the vertices for an arrow head polygon without drawing.

## Usage

``` r
arrow_head_points(x, y, angle, size, arrow_angle = pi/6)
```

## Arguments

- x:

  Arrow tip x coordinate.

- y:

  Arrow tip y coordinate.

- angle:

  Angle of incoming edge (radians).

- size:

  Arrow size.

- arrow_angle:

  Arrow head angle in radians. Default pi/6 (30 degrees).

## Value

List with x, y vectors and midpoint coordinates.
