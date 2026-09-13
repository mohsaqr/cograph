# Reorder the Nodes of a Network

Changes the order the nodes are stored in, which is the order plotting
functions lay them out in. The network itself is unchanged.

## Usage

``` r
reorder_nodes(x, order, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- order:

  Node labels or indices, in the wanted order, or one of `"label"`,
  `"degree"`, `"strength"` to sort by. Sorting by a measure is
  descending.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the nodes in the requested order and edge
indices remapped, or the input format when `keep_format = TRUE`.

## See also

[`rename_nodes`](https://sonsoles.me/cograph/reference/rename_nodes.md),
[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

get_labels(reorder_nodes(adj, order = "degree"))
#> [1] "A" "B" "C" "D"
get_labels(reorder_nodes(adj, order = c("D", "C", "B", "A")))
#> [1] "D" "C" "B" "A"
```
