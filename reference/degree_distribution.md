# Degree Distribution Visualization

Plots a histogram or a complementary cumulative distribution of node
degrees. When the degree range is at most 50, the default bins are
integer-aligned, with one bar per degree value.

## Usage

``` r
degree_distribution(
  x,
  mode = "all",
  directed = NULL,
  loops = TRUE,
  simplify = "sum",
  cumulative = FALSE,
  breaks = NULL,
  bins = NULL,
  bin_width = NULL,
  normalize = FALSE,
  log = "",
  main = "Degree Distribution",
  xlab = "Degree",
  ylab = NULL,
  col = "steelblue",
  border = "white",
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- mode:

  For directed networks: "all", "in", or "out". Default "all".

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- loops:

  Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.

- simplify:

  How to combine multiple edges between the same node pair. Options:
  "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
  multiple edges.

- cumulative:

  Logical. If TRUE, show CCDF (complementary cumulative distribution:
  P(degree \>= k)) instead of frequency. Default FALSE.

- breaks:

  Bin specification passed to
  [`hist`](https://rdrr.io/r/graphics/hist.html). Can be a numeric
  vector of breakpoints, a single number giving the number of bins, or a
  character string naming an algorithm (e.g. "Sturges", "FD", "scott").
  Overrides `bins` and `bin_width`. Default NULL (auto-detect).

- bins:

  Integer. Number of equal-width bins spanning the degree range.
  Overrides `bin_width`. Default NULL.

- bin_width:

  Numeric. Width of each bin. Default NULL, which uses a width of 1 when
  the degree range is at most 50 and the Freedman-Diaconis rule
  otherwise.

- normalize:

  Logical. If TRUE, the y-axis shows proportions (bars sum to 1) instead
  of counts. Default FALSE.

- log:

  Character. Axis log-scaling: "" (none, default), "x", "y", or "xy".
  Histogram plots apply y-axis log scaling for "y" or "xy"; cumulative
  plots support x, y, and xy scaling, and "xy" gives a log-log CCDF.

- main:

  Character. Plot title. Default "Degree Distribution".

- xlab:

  Character. X-axis label. Default "Degree".

- ylab:

  Character. Y-axis label. Default auto-chosen based on `normalize` and
  `cumulative`.

- col:

  Character. Bar fill or line color. Default "steelblue".

- border:

  Character. Bar border color. Default "white".

- ...:

  Additional graphical arguments passed to
  [`barplot`](https://rdrr.io/r/graphics/barplot.html) (histogram) or
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) (cumulative).

## Value

Invisibly returns a list with components:

- degree:

  Named numeric vector of per-node degrees.

- table:

  Table of degree frequencies.

- breaks:

  Breakpoints of the degree histogram.

- counts:

  Bin counts.

- proportions:

  Bin proportions (`counts / sum(counts)`).

The same five components are returned for the histogram and the
cumulative plot. The bin components always describe the histogram bins.

## Examples

``` r
cograph::degree_distribution(regulation_net)
```
