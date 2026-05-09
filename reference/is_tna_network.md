# Check if Network is TNA-based

Checks whether a cograph_network was created from a tna or group_tna
object.

## Usage

``` r
is_tna_network(x)
```

## Arguments

- x:

  A CographNetwork or cograph_network object.

## Value

Logical: TRUE if the network was created from a TNA object, FALSE
otherwise.

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
# Non-TNA network
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
is_tna_network(net)  # FALSE
#> [1] FALSE

model <- tna::tna(tna::group_regulation)
net_tna <- as_cograph(model)
is_tna_network(net_tna)  # TRUE
#> [1] TRUE
```
