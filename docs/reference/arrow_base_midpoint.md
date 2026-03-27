# Calculate Arrow Base Midpoint

Returns the midpoint between the arrow wings (where the curve should
end). This is used to connect the edge line to the back of the arrow
head.

## Usage

``` r
arrow_base_midpoint(x, y, angle, size, arrow_angle = pi/6)
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

## Value

List with x, y coordinates of the arrow base midpoint.
