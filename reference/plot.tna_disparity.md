# Plot Disparity Filter Result

Plot Disparity Filter Result

## Usage

``` r
# S3 method for class 'tna_disparity'
plot(x, type = c("backbone", "comparison"), ...)
```

## Arguments

- x:

  A tna_disparity object.

- type:

  Plot type: "backbone" (default) or "comparison".

- ...:

  Additional arguments passed to splot.

## Value

Invisibly returns the value from the underlying
[`splot`](https://sonsoles.me/cograph/reference/splot.md) call. Called
primarily for the side effect of producing a plot.

## Examples

``` r
mat <- matrix(c(0.0, 0.5, 0.1, 0.0, 0.3, 0.0, 0.4, 0.1,
                0.1, 0.2, 0.0, 0.5, 0.0, 0.1, 0.3, 0.0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
disp <- disparity_filter(cograph(mat), level = 0.05)
plot(disp)

```
