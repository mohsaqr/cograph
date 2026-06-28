# Convert Network to igraph Object

Converts various network representations to an igraph object. Supports
matrices, edge-list data frames, igraph objects, network objects,
cograph_network, and tna objects.

## Usage

``` r
to_igraph(x, directed = NULL)
```

## Arguments

- x:

  Network input. Can be:

  - A square numeric matrix (adjacency/weight matrix)

  - A data frame edge list with source and target columns

  - An igraph object (returned as-is or converted if directed differs)

  - A statnet network object

  - A cograph_network object

  - A tna object

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

## Value

An igraph object.

## See also

[`to_data_frame`](https://sonsoles.me/cograph/reference/to_data_frame.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
# From matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
g <- to_igraph(adj)

# Force directed
g_dir <- to_igraph(adj, directed = TRUE)
```
