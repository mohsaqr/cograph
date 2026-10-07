# Plot Degree-Degree Correlation

Scatter plot of each node's degree against the average degree of its
neighbors. A positive slope indicates assortative mixing and a negative
slope indicates disassortative mixing. When more than two nodes have
neighbors, a least-squares line is added and the Pearson correlation is
printed in the top margin.

## Usage

``` r
plot_degree_correlation(
  x,
  mode = "all",
  directed = NULL,
  col = "steelblue",
  main = "Degree-Degree Correlation",
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna.

- mode:

  Character. Degree type and neighborhood used for directed networks:
  `"all"` (default), `"in"`, or `"out"`.

- directed:

  Logical or NULL. Default NULL (auto-detect).

- col:

  Point color. Default `"steelblue"`.

- main:

  Title. Default `"Degree-Degree Correlation"`.

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly, a data frame with one row per node and columns `node`,
`degree` and `avg_neighbor_degree`. The average neighbor degree is `NA`
for nodes without neighbors.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`degree_distribution`](https://sonsoles.me/cograph/reference/degree_distribution.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
cograph::plot_degree_correlation(student_interactions)
```
