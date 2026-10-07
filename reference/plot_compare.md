# Plot Network Difference (alias of plot_difference)

`plot_compare()` is an alias of
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
and calls the same implementation.
[`tna::plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.html)
calls it by name.
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
is the preferred name.

## Usage

``` r
plot_compare(x, ...)
```

## Arguments

- x:

  First network (see
  [`plot_difference`](https://sonsoles.me/cograph/reference/plot_difference.md)).

- ...:

  Arguments passed to
  [`plot_difference`](https://sonsoles.me/cograph/reference/plot_difference.md).

## Value

Invisibly, the value of
[`plot_difference`](https://sonsoles.me/cograph/reference/plot_difference.md).

## See also

[`plot_difference`](https://sonsoles.me/cograph/reference/plot_difference.md)

## Examples

``` r
plot_compare(regulation_net, t(regulation_net))
```
