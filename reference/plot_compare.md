# Plot Network Difference (alias of plot_difference)

`plot_compare()` is an alias of
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md).
It is **not deprecated**:
[`tna::plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md)
delegates to it by name (`cograph::plot_compare(x, y, ...)`), so the
alias is part of the tna integration and must keep working. New cograph
code may prefer the
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
name; both call the same implementation.

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
m1 <- matrix(stats::runif(25), 5, 5)
m2 <- matrix(stats::runif(25), 5, 5)
rownames(m1) <- colnames(m1) <- LETTERS[1:5]
rownames(m2) <- colnames(m2) <- LETTERS[1:5]
plot_compare(m1, m2)
```
