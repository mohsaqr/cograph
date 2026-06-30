# Plot Network Difference (deprecated)

**Deprecated**: use
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
instead. This name collided with
[`tna::plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md)
(an S3 generic for comparing tna models), so the cograph
difference-network plotter was renamed to
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md).
`plot_compare()` remains as a thin wrapper for backward compatibility
and will be removed in a future release.

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
suppressWarnings(plot_compare(m1, m2))  # warns: use plot_difference()
```
