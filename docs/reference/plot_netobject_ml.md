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
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly returns `x`.
