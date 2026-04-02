# qgraph Point on Node Boundary

Simplified boundary calculation for splot that approximates qgraph
behavior while working with cograph's coordinate system.

## Usage

``` r
qgraph_cent_to_edge_simple(x, y, angle, node_size, shape = "circle")
```

## Arguments

- x:

  Node center x coordinate.

- y:

  Node center y coordinate.

- angle:

  Angle to target (radians).

- node_size:

  Node radius in user coordinates.

- shape:

  Node shape.

## Value

List with x, y coordinates on boundary.
