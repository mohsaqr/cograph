# ggplot2 Conversion

Convert Cograph network to ggplot2 object.

## Value

A ggplot2 object representing the network.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
p <- sn_ggplot(adj)
```
