# Get Number of Edges

Returns the number of edges in a cograph_network.

## Usage

``` r
n_edges(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

Integer: number of edges.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`n_nodes`](http://sonsoles.me/cograph/reference/n_nodes.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
n_edges(net)  # 3
#> [1] 3
```
