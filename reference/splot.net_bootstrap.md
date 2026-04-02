# Plot Nestimate Bootstrap Results

Visualizes `net_bootstrap` objects from the Nestimate package. Mirrors
`splot.tna_bootstrap` but adapts to Nestimate's field layout: weights
live under `$original$weights`, directed is not always TRUE, and there
are no donut/inits.

## Usage

``` r
splot.net_bootstrap(
  x,
  display = c("styled", "significant", "full"),
  show_ci = FALSE,
  show_stars = TRUE,
  inherit_style = TRUE,
  ...
)
```

## Arguments

- x:

  A `net_bootstrap` object (from Nestimate).

- display:

  Display mode: `"styled"` (default), `"significant"`, or `"full"`.

- show_ci:

  Logical: overlay CI bounds on edge labels? Default FALSE.

- show_stars:

  Logical: show significance stars on edge labels? Default FALSE.

- inherit_style:

  Logical: inherit labels/layout/colors from network? Default TRUE.

- ...:

  Additional arguments passed to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly returns the plot.
