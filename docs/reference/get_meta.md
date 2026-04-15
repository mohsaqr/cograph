# Get Metadata from Cograph Network

Extracts the consolidated metadata list from a cograph_network object.
The metadata contains source type, layout info, and TNA metadata.

## Usage

``` r
get_meta(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

A list with components:

- `source`:

  Character string indicating input type

- `layout`:

  List with layout name and seed, or NULL

- `tna`:

  List with TNA metadata (type, group_name, group_index), or NULL

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`get_source`](https://sonsoles.me/cograph/reference/get_source.md)

## Examples

``` r
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_meta(net)
#> $source
#> [1] "matrix"
#> 
```
