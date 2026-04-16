# Subdivide Polygon Edges

Interpolates extra points along each edge of a polygon so that
vertex-count-based fill calculations produce smooth, accurate
proportions.

## Usage

``` r
subdivide_polygon(verts, n_per_edge = 25)
```

## Arguments

- verts:

  List with `x`, `y` vectors of corner vertices.

- n_per_edge:

  Number of subdivisions per edge.

## Value

List with `x`, `y` vectors (n_corners \* n_per_edge points).
