# Inset Polygon Vertices

Creates an inner polygon by scaling vertices toward the centroid.

## Usage

``` r
inset_polygon_vertices(outer, inner_ratio)
```

## Arguments

- outer:

  List with x, y vectors of outer polygon vertices.

- inner_ratio:

  Ratio to scale vertices toward center (0-1).

## Value

List with x, y vectors of inner polygon vertices.
