# Get Polygon Vertices by Shape Name

Returns outer polygon vertices for donut ring shapes. Non-circle shapes
are subdivided so that fill proportions are accurate.

## Usage

``` r
get_donut_base_vertices(shape, x, y, r)
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

## Value

List with x, y vectors of vertices.
