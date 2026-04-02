# Set Layout in Cograph Network

Sets the layout coordinates in a cograph_network object. Updates the x
and y columns in the nodes data frame.

## Usage

``` r
set_layout(x, layout_df)
```

## Arguments

- x:

  A cograph_network object.

- layout_df:

  A data frame with x and y columns, or a matrix with 2 columns.

## Value

The modified cograph_network object.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md),
[`sn_layout`](http://sonsoles.me/cograph/reference/sn_layout.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
layout <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
net <- set_layout(net, layout)
get_nodes(net)
#>   id label name   x y
#> 1  1     1    1 0.0 0
#> 2  2     2    2 1.0 0
#> 3  3     3    3 0.5 1
```
