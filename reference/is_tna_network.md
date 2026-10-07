# Check if Network is TNA-based

Checks whether a cograph_network was created from a tna object, such as
one model of a group_tna object.

## Usage

``` r
is_tna_network(x)
```

## Arguments

- x:

  A CographNetwork or cograph_network object.

## Value

Logical. `TRUE` if the network was created from a tna object, `FALSE`
otherwise, including for any input that is not a network.

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
is_tna_network(as_cograph(regulation_net))
#> [1] FALSE
```
