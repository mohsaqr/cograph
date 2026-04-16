# Plot Network Motifs

Visualize motif frequencies and their statistical significance.

## Usage

``` r
# S3 method for class 'cograph_motifs'
plot(
  x,
  type = c("bar", "heatmap", "network"),
  show_nonsig = FALSE,
  top_n = NULL,
  colors = c("#2166AC", "#F7F7F7", "#B2182B"),
  ...
)
```

## Arguments

- x:

  A `cograph_motifs` object from
  [`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)

- type:

  Plot type:

  `"bar"`

  :   (default) Bar chart of motif frequencies, colored by significance
      direction (over/under-represented).

  `"heatmap"`

  :   Heatmap of z-scores across motif types.

  `"network"`

  :   Network diagrams of the top motifs by \|z-score\|.

- show_nonsig:

  Show non-significant motifs? Default FALSE.

- top_n:

  Show only top N motifs by \|z-score\|. Default NULL (all).

- colors:

  Three-element color vector for under-represented, neutral, and
  over-represented motifs. Default `c("#2166AC", "#999999", "#B2182B")`
  (blue/gray/red).

- ...:

  Additional arguments passed to plotting functions

## Value

A ggplot2 object (invisibly)

## See also

[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
for the analysis that produces this object

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
mat <- matrix(sample(0:1, 100, replace = TRUE, prob = c(0.7, 0.3)), 10, 10)
diag(mat) <- 0
m <- motif_census(mat, directed = TRUE, n_random = 50)
plot(m)

plot(m, type = "network")

```
