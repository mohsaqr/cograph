# Plot Edge Weight Distribution

Histogram of edge weights in a network. The number of edges and the mean
and standard deviation of the weights are printed in the top margin.

## Usage

``` r
plot_edge_weights(
  x,
  normalize = FALSE,
  bins = NULL,
  log = "",
  directed = NULL,
  col = "steelblue",
  border = "white",
  main = "Edge Weight Distribution",
  xlab = "Weight",
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna.

- normalize:

  Logical. Show proportions. Default FALSE.

- bins:

  Integer or NULL. Number of equal-width bins. With the default NULL,
  integer weights spanning at most 30 units get one bin per integer and
  other weights use the Freedman-Diaconis rule.

- log:

  Character. `"y"` or `"xy"` log-scales the y-axis. Any other value
  gives linear axes. Default `""`.

- directed:

  Logical or NULL. Default NULL (auto-detect).

- col:

  Fill color. Default `"steelblue"`.

- border:

  Border color. Default `"white"`.

- main:

  Title. Default `"Edge Weight Distribution"`.

- xlab:

  X-axis label. Default `"Weight"`.

- ...:

  Additional arguments passed to
  [`barplot`](https://rdrr.io/r/graphics/barplot.html).

## Value

Invisibly, a numeric vector of edge weights (all 1 for an unweighted
network).

## Examples

``` r
cograph::plot_edge_weights(regulation_net)
```
