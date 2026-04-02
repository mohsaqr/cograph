# Convert Network to statnet network Object

Converts any supported network format to a statnet network object.

## Usage

``` r
to_network(x, directed = NULL)
```

## Arguments

- x:

  Network input: matrix, cograph_network, igraph, tna, etc.

- directed:

  Logical or NULL. If NULL (default), auto-detect from input.

## Value

A network object from the network package.

## See also

[`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md),
[`to_matrix`](http://sonsoles.me/cograph/reference/to_matrix.md),
[`to_df`](http://sonsoles.me/cograph/reference/to_data_frame.md),
[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
if (requireNamespace("network", quietly = TRUE)) {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  net <- to_network(adj)
}
```
