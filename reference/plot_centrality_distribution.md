# Plot Centrality Distribution

Histogram or density plot of one centrality measure. The input is either
the data frame returned by
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) or a
network, for which the measure is computed first.

## Usage

``` r
plot_centrality_distribution(
  x,
  measure = "degree_all",
  type = c("histogram", "density"),
  normalize = FALSE,
  bins = NULL,
  log = "",
  col = "steelblue",
  border = "white",
  main = NULL,
  xlab = NULL,
  ...
)
```

## Arguments

- x:

  A data frame from
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  or a network input (matrix, igraph, cograph_network, tna).

- measure:

  Character. Column of the centrality output to plot, for example
  `"degree_all"` (default) or `"strength_in"`. An unknown name is an
  error that lists the available columns.

- type:

  Character. `"histogram"` (default) or `"density"`.

- normalize:

  Logical. Show proportions instead of counts. Default FALSE.

- bins:

  Integer or NULL. Number of equal-width histogram bins. The default
  NULL uses the Freedman-Diaconis rule.

- log:

  Character. `"y"` or `"xy"` log-scales the y-axis. The x-axis is never
  log-scaled, and any other value gives linear axes. Default `""`.

- col:

  Fill color (line color for the density). Default `"steelblue"`.

- border:

  Bar border color for the histogram. Default `"white"`.

- main:

  Plot title. The default NULL builds a title such as
  `"Degree Distribution"` from the measure name.

- xlab:

  X-axis label. The default NULL uses the measure name.

- ...:

  Additional arguments passed to
  [`barplot`](https://rdrr.io/r/graphics/barplot.html) or
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly, a numeric vector of the finite centrality values plotted.

## Examples

``` r
cograph::plot_centrality_distribution(regulation_net, measure = "strength_all")
```
