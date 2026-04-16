# Plot Edge Weight Distribution

Histogram of edge weights in a network.

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

  Integer or NULL. Number of bins. Default NULL (auto).

- log:

  Character. Log scaling. Default `""`.

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

Invisibly returns the weight vector.

## Examples

``` r
adj <- matrix(c(0, 2, 3, 2, 0, 1, 3, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
cograph::plot_edge_weights(adj)
```
