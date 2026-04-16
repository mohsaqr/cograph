# Grid Rendering

Main grid-based rendering functions.

## Value

See individual functions:
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) returns a
`cograph_network` object invisibly;
[`sn_ggplot`](https://sonsoles.me/cograph/reference/sn_ggplot.md)
returns a ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
soplot(adj)
} # }
```
