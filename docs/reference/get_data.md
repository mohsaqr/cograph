# Get Original Data from Cograph Network

Extracts the original estimation data stored in a cograph_network
object. This is the raw input data (e.g., sequence matrix from tna, edge
list data frame) preserved for reference.

## Usage

``` r
get_data(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

The original data object, or NULL if not stored.

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`get_meta`](https://sonsoles.me/cograph/reference/get_meta.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_data(net)  # NULL (matrices don't store raw data)
#> NULL
```
