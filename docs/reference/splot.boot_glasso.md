# Plot Nestimate GLASSO Bootstrap Results

Visualizes `boot_glasso` objects from the Nestimate package. Plots a
partial-correlation network with edge inclusion probabilities mapped to
edge transparency.

## Usage

``` r
splot.boot_glasso(
  x,
  use_thresholded = TRUE,
  show_inclusion = TRUE,
  inclusion_threshold = NULL,
  edge_positive_color = "#2E7D32",
  edge_negative_color = "#C62828",
  ...
)
```

## Arguments

- x:

  A `boot_glasso` object (from Nestimate).

- use_thresholded:

  Logical: use `$thresholded_pcor`? If FALSE, uses `$original_pcor`.
  Default TRUE.

- show_inclusion:

  Logical: scale edge alpha by inclusion probability? Default TRUE.

- inclusion_threshold:

  Numeric: minimum inclusion probability to show an edge. Default
  `1 - x$alpha` (i.e. the complement of the alpha level).

- edge_positive_color:

  Color for positive partial correlations. Default `"#2E7D32"`.

- edge_negative_color:

  Color for negative partial correlations. Default `"#C62828"`.

- ...:

  Additional arguments passed to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly returns the plot.
