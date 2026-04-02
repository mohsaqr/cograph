# qgraph Cent2Edge (EXACT - critical formula)

Calculates the point on node boundary where an edge should connect. This
is qgraph's exact formula for positioning arrows and edge endpoints.

## Usage

``` r
qgraph_cent2edge(x, y, cex, offset = 0, angle, plot_info = NULL)
```

## Arguments

- x:

  Node center x coordinate.

- y:

  Node center y coordinate.

- cex:

  Node size (vsize value, not yet scaled).

- offset:

  Additional offset distance.

- angle:

  Angle from node center to target point (radians).

- plot_info:

  Plot dimension info from qgraph_plot_info(). NULL to auto-compute.

## Value

List with x, y coordinates on node boundary.
