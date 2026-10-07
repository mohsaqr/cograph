# Plot Centrality Heatmap

Plots a heatmap of nodes (rows) by centrality measures (columns). Cell
fill is the z-score of each value within its measure, mapped to a
diverging color scale. Optional row clustering places nodes with similar
centrality profiles next to each other.

## Usage

``` r
plot_centrality_heatmap(
  x,
  measures = NULL,
  cluster_rows = TRUE,
  order_by = NULL,
  show_values = FALSE,
  value_digits = 1L,
  low = "#2171B5",
  mid = "white",
  high = "#CB181D",
  limits = c(-2.5, 2.5),
  title = NULL,
  subtitle = "z-scored within measure",
  ...
)
```

## Arguments

- x:

  Centrality data frame (from
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md))
  or a network input.

- measures:

  Character vector of measure names. When `x` is a network and
  `measures` is NULL (default), degree, strength, betweenness, closeness
  and eigenvector are computed. When `x` is a centrality data frame,
  NULL keeps all its measure columns.

- cluster_rows:

  Logical. Order rows by hierarchical clustering (Euclidean distance,
  average linkage) of the z-scored profiles. Applied when there are more
  than two nodes. Default TRUE.

- order_by:

  Used when rows are not clustered. Name of the measure that sorts rows
  in descending order. NULL (default) or an unknown name uses the first
  measure.

- show_values:

  Logical. Print the raw centrality values in the cells. Default FALSE.

- value_digits:

  Decimal places for cell values. Default 1.

- low, mid, high:

  Color stops for the diverging scale. Defaults to blue, white and red.

- limits:

  Numeric c(min, max) z-score range. Values outside are squished to the
  endpoints. Default c(-2.5, 2.5).

- title, subtitle:

  Plot title and subtitle. The default subtitle is "z-scored within
  measure".

- ...:

  Passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  when `x` is a network.

## Value

A ggplot object.

## Examples

``` r
plot_centrality_heatmap(regulation_net)
```
