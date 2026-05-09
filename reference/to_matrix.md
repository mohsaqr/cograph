# Convert Network to Adjacency Matrix

Converts any supported network format to an adjacency matrix.

## Usage

``` r
to_matrix(x, directed = NULL)
```

## Arguments

- x:

  Network input: matrix, cograph_network, igraph, network, tna, etc.

- directed:

  Logical or NULL. If NULL (default), auto-detect from input.

## Value

A square numeric adjacency matrix, preserving row/column names when
available.

## See also

[`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
[`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`to_network`](https://sonsoles.me/cograph/reference/to_network.md)

## Examples

``` r
# From matrix
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
to_matrix(adj)
#>     A   B   C   D
#> A 0.0 0.5 0.8 0.0
#> B 0.5 0.0 0.3 0.6
#> C 0.8 0.3 0.0 0.4
#> D 0.0 0.6 0.4 0.0

# From cograph_network
net <- as_cograph(adj)
to_matrix(net)
#>     A   B   C   D
#> A 0.0 0.5 0.8 0.0
#> B 0.5 0.0 0.3 0.6
#> C 0.8 0.3 0.0 0.4
#> D 0.0 0.6 0.4 0.0

# From igraph (weighted graph)
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
  to_matrix(g)
}
#>     A   B   C   D
#> A 0.0 0.5 0.8 0.0
#> B 0.5 0.0 0.3 0.6
#> C 0.8 0.3 0.0 0.4
#> D 0.0 0.6 0.4 0.0
```
