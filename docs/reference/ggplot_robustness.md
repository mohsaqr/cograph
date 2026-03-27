# Compare Network Robustness (ggplot2)

Creates a ggplot2 faceted visualization comparing robustness across
multiple networks. Produces publication-quality figures similar to those
in Nature Scientific Reports.

## Usage

``` r
ggplot_robustness(
  ...,
  networks = NULL,
  measures = c("betweenness", "degree", "random"),
  strategy = "sequential",
  colors = NULL,
  title = NULL,
  n_iter = 1000,
  seed = NULL,
  type = "vertex",
  ncol = NULL,
  free_y = FALSE
)
```

## Arguments

- ...:

  Named arguments: network names as names, network objects as values.

- networks:

  Named list of networks (alternative to ...).

- measures:

  Attack strategies to compare. Default c("betweenness", "degree",
  "random").

- strategy:

  Character string; "sequential" (default) recalculates centrality after
  each removal, "static" uses initial centrality ranking throughout.

- colors:

  Named vector of colors for measures.

- title:

  Overall title. Default NULL.

- n_iter:

  Iterations for random. Default 1000.

- seed:

  Random seed. Default NULL.

- type:

  Removal type. Default "vertex".

- ncol:

  Columns in facet. Default NULL (auto).

- free_y:

  If TRUE, allow different y-axis scales per facet. Default FALSE.

## Value

A ggplot2 object.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("ggplot2", quietly = TRUE)) {

  g1 <- igraph::sample_pa(40, m = 2, directed = FALSE)
  g2 <- igraph::sample_gnp(40, 0.15)

  ggplot_robustness(
    "Teaching network" = g1,
    "Collaborative network" = g2,
    n_iter = 20
  )
}
```
