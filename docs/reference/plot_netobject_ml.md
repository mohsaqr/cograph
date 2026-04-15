# Plot a Multilevel Nestimate netobject

Creates a side-by-side plot for a `netobject_ml` object, showing the
between-person and within-person networks.

## Usage

``` r
plot_netobject_ml(
  x,
  layout = NULL,
  common_scale = TRUE,
  titles = c("Between-person", "Within-person"),
  ...
)

# S3 method for class 'netobject_ml'
plot(x, ...)
```

## Arguments

- x:

  A `netobject_ml` object with `$between` and `$within` networks.

- layout:

  Character: layout algorithm. Default `"oval"` (deterministic).

- common_scale:

  Logical: use the same maximum weight for both panels? Default TRUE.

- titles:

  Character vector of length 2: panel titles. Default
  `c("Between-person", "Within-person")`.

- ...:

  Additional arguments passed to
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly returns `x`.

## Examples

``` r
mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
colnames(mat) <- rownames(mat) <- c("A", "B", "C")
btw <- as_cograph(mat)
wth <- as_cograph(mat * 0.6)
ml <- structure(list(between = btw, within = wth), class = c("netobject_ml", "list"))
plot_netobject_ml(ml)
```
