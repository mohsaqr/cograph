# Create Unified cograph_network Object

Internal constructor that creates a cograph_network object with the
unified format. Both cograph() and as_cograph() use this to ensure
identical output.

## Usage

``` r
.create_cograph_network(
  nodes,
  edges,
  directed,
  meta = list(),
  weights = NULL,
  data = NULL,
  node_groups = NULL,
  type = NULL
)
```

## Arguments

- nodes:

  Data frame with node information (id, label, x, y, ...).

- edges:

  Data frame with edge information (from, to, weight).

- directed:

  Logical. Is the network directed?

- meta:

  List with consolidated metadata: source, layout, tna sub-fields.

- weights:

  Full n×n weight matrix for TNA compatibility, or NULL.

- data:

  Original estimation data (sequence matrix, edge list, etc.), or NULL.

- node_groups:

  Optional node groupings data frame.

## Value

A cograph_network object (named list with class).
