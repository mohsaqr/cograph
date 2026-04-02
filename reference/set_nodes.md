# Set Nodes in Cograph Network

Replaces the nodes data frame in a cograph_network object.

## Usage

``` r
set_nodes(x, nodes_df)
```

## Arguments

- x:

  A cograph_network object.

- nodes_df:

  A data frame with node information (id, label columns expected).

## Value

The modified cograph_network object.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md),
[`set_edges`](http://sonsoles.me/cograph/reference/set_edges.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
new_nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
net <- set_nodes(net, new_nodes)
get_labels(net)
#> [1] "A" "B" "C"
```
