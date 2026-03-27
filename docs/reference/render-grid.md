# Grid Rendering

Main grid-based rendering functions.

## Value

See individual functions:
[`soplot`](http://sonsoles.me/cograph/reference/soplot.md) returns a
`cograph_network` object invisibly;
[`sn_ggplot`](http://sonsoles.me/cograph/reference/sn_ggplot.md) returns
a ggplot2 object.

## Examples

``` r
# \donttest{
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
soplot(adj)

# }
```
