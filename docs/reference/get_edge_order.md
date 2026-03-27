# Get Edge Rendering Order

Returns indices for rendering edges from weakest to strongest.

## Usage

``` r
get_edge_order(edges, priority = NULL)
```

## Arguments

- edges:

  Edge data frame.

- priority:

  Optional numeric vector of edge priorities. Higher = on top.

## Value

Integer vector of indices.
