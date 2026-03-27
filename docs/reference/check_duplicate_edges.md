# Check and Handle Duplicate Edges

Detects duplicate edges in undirected networks and either errors with
guidance or aggregates them per the user's `edge_duplicates` setting.

## Usage

``` r
check_duplicate_edges(edges, directed, edge_duplicates)
```

## Arguments

- edges:

  Edge data frame.

- directed:

  Logical: is the network directed?

- edge_duplicates:

  Aggregation method (NULL to error, or "sum"/"mean"/etc).

## Value

Possibly aggregated edge data frame.
