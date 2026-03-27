# Get Labels from Cograph Network

Extracts the node labels vector from a cograph_network object.

## Usage

``` r
get_labels(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

A character vector of node labels.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_labels(net)
#> [1] "1" "2" "3"
```
