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

A data frame with columns: from, to, weight.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`n_edges`](http://sonsoles.me/cograph/reference/n_edges.md),
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md)

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
