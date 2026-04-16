# Set Edges in Cograph Network

Replaces the edges in a cograph_network object. Expects a data frame
with from, to, and optionally weight columns.

## Usage

``` r
set_edges(x, edges_df)
```

## Arguments

- x:

  A cograph_network object.

- edges_df:

  A data frame with columns: from, to, and optionally weight.

## Value

The modified cograph_network object.

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`get_edges`](https://sonsoles.me/cograph/reference/get_edges.md),
[`set_nodes`](https://sonsoles.me/cograph/reference/set_nodes.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
net <- set_edges(net, new_edges)
get_edges(net)
#>   from to weight
#> 1    1  2    0.5
#> 2    2  3    0.8
```
