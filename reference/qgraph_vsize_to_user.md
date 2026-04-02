# qgraph Node Size to User Coordinates

Converts qgraph vsize to user coordinate radius using qgraph's exact
logic.

## Usage

``` r
qgraph_vsize_to_user(vsize, plot_info = NULL)
```

## Arguments

- vsize:

  Node size value (as used in qgraph).

- plot_info:

  Plot dimension info. NULL to auto-compute.

## Value

Node radius in user coordinates.
