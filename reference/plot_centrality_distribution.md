# Plot Centrality Distribution

Histogram or density plot of any centrality measure. Accepts the output
of [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
directly.

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

  Character. Which centrality measure to plot. Default `"degree_all"`.
  Must match a column name in the centrality output.

- type:

  Character. `"histogram"` (default) or `"density"`.

- normalize:

  Logical. Show proportions instead of counts. Default FALSE.

- bins:

  Integer or NULL. Number of bins. Default NULL (auto).

- log:

  Character. Log scaling: `""`, `"y"`, or `"xy"`. Values containing
  `"x"` are accepted for compatibility but only the y-axis is log-scaled
  by this plotting implementation. Default `""`.

- col:

  Fill color. Default `"steelblue"`.

- border:

  Border color. Default `"white"`.

- main:

  Plot title. Default auto-generated from measure name.

- xlab:

  X-axis label. Default auto-generated.

- ...:

  Additional arguments passed to
  [`barplot`](https://rdrr.io/r/graphics/barplot.html) or
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns the centrality values plotted.

## Examples

``` r
adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
rownames(adj) <- colnames(adj) <- LETTERS[1:4]
cograph::plot_centrality_distribution(adj, measure = "degree_all")
```
