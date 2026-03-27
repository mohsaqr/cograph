# Calculate Point on Node Boundary

Given a node center, size, and angle, calculates the point on the node
boundary. Works with various shapes.

## Usage

``` r
cent_to_edge(x, y, angle, cex, cex2 = NULL, shape = "circle")
```

## Arguments

- x:

  Node center x coordinate.

- y:

  Node center y coordinate.

- angle:

  Angle in radians.

- cex:

  Node size (radius in user coordinates).

- cex2:

  Secondary size for ellipse width (NULL for square aspect).

- shape:

  Node shape: "circle", "square", "ellipse", or polygon name.

## Value

List with x, y coordinates on boundary.
