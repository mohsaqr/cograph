# Calculate Perpendicular Midpoint for Curved Edges

Computes a control point perpendicular to the line between two nodes,
used for xspline() curve generation.

## Usage

``` r
perp_mid(x0, y0, x1, y1, cex, q = 0.5)
```

## Arguments

- x0:

  Start x coordinate.

- y0:

  Start y coordinate.

- x1:

  End x coordinate.

- y1:

  End y coordinate.

- cex:

  Curvature amount (positive = left, negative = right).

- q:

  Position along edge (0 = start, 0.5 = middle, 1 = end).

## Value

List with x, y coordinates of control point.
