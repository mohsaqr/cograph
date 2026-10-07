# Plot Comparison Heatmap

Plots a heatmap of the difference between two weight matrices, or of
either matrix alone. Rows are source nodes and columns are target nodes.

## Usage

``` r
plot_comparison_heatmap(
  x,
  y = NULL,
  type = c("difference", "x", "y"),
  name_x = "x",
  name_y = "y",
  low_color = "blue",
  mid_color = "white",
  high_color = "red",
  limits = NULL,
  show_values = FALSE,
  value_size = 3,
  digits = 2,
  title = NULL,
  xlab = "Target",
  ylab = "Source"
)
```

## Arguments

- x:

  First network: matrix, `cograph_network`, `tna`, `igraph`, or list
  with a matrix `weights` component.

- y:

  Second network, of the same type and dimensions as `x`. Required for
  `type = "difference"` and `type = "y"`; it may be `NULL` for
  `type = "x"`.

- type:

  What to display: `"difference"` (x - y), `"x"`, or `"y"`.

- name_x:

  Label for the first network in the default title.

- name_y:

  Label for the second network in the default title.

- low_color:

  Color for low (negative) values.

- mid_color:

  Color for zero.

- high_color:

  Color for high (positive) values.

- limits:

  Color scale limits. `NULL` uses the data range. Use `c(-1, 1)` for
  normalized values.

- show_values:

  Logical. Display values in cells?

- value_size:

  Text size for cell values.

- digits:

  Decimal places for cell values.

- title:

  Plot title. `NULL` builds one from `type`, `name_x` and `name_y`.

- xlab:

  X-axis label.

- ylab:

  Y-axis label.

## Value

A ggplot object. The color scale is a diverging gradient with its
midpoint at 0.

## Examples

``` r
plot_comparison_heatmap(regulation_net, t(regulation_net))

```
