# Get Number of Nodes

Returns the number of nodes in a cograph_network.

## Usage

``` r
n_nodes(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

Integer: number of nodes.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`n_edges`](http://sonsoles.me/cograph/reference/n_edges.md),
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
n_nodes(net)  # 3
#> [1] 3
```
