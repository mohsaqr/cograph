# Get Nodes from Cograph Network (Deprecated)

Extracts the nodes data frame from a cograph_network object.
**Deprecated**: Use
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md)
instead.

## Usage

``` r
nodes(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

A data frame with columns: id, label, name, x, y (and possibly others).

## See also

[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md),
[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`n_nodes`](http://sonsoles.me/cograph/reference/n_nodes.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
nodes(net)  # Deprecated, use get_nodes(net) instead
#>   id label name  x  y
#> 1  1     1    1 NA NA
#> 2  2     2    2 NA NA
#> 3  3     3    3 NA NA
```
