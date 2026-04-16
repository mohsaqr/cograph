# Plot Degree-Degree Correlation

Scatter plot of each node's degree against the average degree of its
neighbors. Reveals assortative (positive slope) or disassortative
(negative slope) mixing patterns.

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

  Character. For directed networks: `"all"`, `"in"`, or `"out"`. Default
  `"all"`.

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

Invisibly returns a data frame with columns `node`, `degree`,
`avg_neighbor_degree`.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`degree_distribution`](https://sonsoles.me/cograph/reference/degree_distribution.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_pa(100, m = 3, directed = FALSE)
  cograph::plot_degree_correlation(g)
}
} # }
```
