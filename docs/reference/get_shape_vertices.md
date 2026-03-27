# Get Shape Vertices

Dispatch function to get vertices for any supported shape.

## Usage

``` r
get_shape_vertices(shape, x, y, r, r2 = NULL, ...)
```

## Arguments

- shape:

  Shape name.

- x:

  Center x coordinate.

- y:

  Center y coordinate.

- r:

  Radius/size.

- r2:

  Secondary radius (for ellipse, rectangle).

- ...:

  Additional shape-specific parameters.

## Value

List with x, y vectors of vertices.
