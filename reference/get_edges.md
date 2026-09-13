# Get Edges from Cograph Network

Extracts the edges data frame from a cograph_network object.

## Usage

``` r
get_edges(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

A data frame with one row per edge and columns `from` and `to` (integer
row numbers into the node table, *not* labels) and `weight`, plus any
extra edge columns the network carries. An undirected network stores one
row per unordered pair. Use
[`as.data.frame.cograph_network`](https://sonsoles.me/cograph/reference/as.data.frame.cograph_network.md)
or [`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md) for
the same table with the endpoints given as node labels.

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`n_edges`](https://sonsoles.me/cograph/reference/n_edges.md),
[`get_nodes`](https://sonsoles.me/cograph/reference/get_nodes.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_edges(net)
#>   from to weight
#> 1    1  2      1
#> 2    1  3      1
#> 3    2  3      1
```
