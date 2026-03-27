# Get Source Type from Cograph Network

Extracts the source type string from a cograph_network object's
metadata.

## Usage

``` r
get_source(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

A character string indicating the input type (e.g., "matrix", "tna",
"igraph", "edgelist"), or "unknown" if not set.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md),
[`get_meta`](http://sonsoles.me/cograph/reference/get_meta.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_source(net)  # "matrix"
#> [1] "matrix"
```
