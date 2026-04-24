# Plot Motif Analysis Results

Create visualizations for motif analysis results including network
diagrams of triads, bar plots of type distributions, and significance
plots.

## Usage

``` r
# S3 method for class 'cograph_motif_analysis'
plot(
  x,
  type = c("triads", "types", "significance", "patterns"),
  n = 20,
  colors = c("#2166AC", "#B2182B"),
  res = 72,
  node_size = 5,
  label_size = 7,
  title_size = 7,
  stats_size = 5,
  ncol = 5,
  legend = TRUE,
  color = "#800020",
  spacing = 1,
  ...
)
```

## Arguments

- x:

  A `cograph_motif_analysis` object from
  [`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)

- type:

  Plot type:

  `"triads"`

  :   (default) Network diagrams of specific named triads, arranged in a
      grid. Each cell shows the three nodes and their edges.

  `"types"`

  :   Bar chart of MAN type frequencies.

  `"significance"`

  :   Z-score plot showing over- and under-represented types. Requires
      `significance = TRUE` in
      [`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md).

  `"patterns"`

  :   Abstract MAN pattern diagrams showing edge structure of each triad
      type without specific node labels.

- n:

  Number of triads/patterns to show. Default 20.

- colors:

  Two-element color vector for the types/significance plots: first color
  for over-represented, second for under-represented. Default
  `c("#2166AC", "#B2182B")` (blue/red).

- res:

  Resolution for scaling (kept for backwards compatibility). Default 72.

- node_size:

  Size of nodes in triad diagrams (1-10 scale). Default 5.

- label_size:

  Font size for node labels (3-letter abbreviations). Default 7.

- title_size:

  Font size for motif type title (e.g., "120C"). Default 7.

- stats_size:

  Font size for statistics text (n, z, p). Default 5.

- ncol:

  Number of columns in the plot grid. Default 5.

- legend:

  Show abbreviation legend at bottom? Default TRUE.

- color:

  Color for nodes, edges, and labels in triad diagrams. Default
  `"#800020"` (maroon).

- spacing:

  Spacing multiplier between grid cells (0.5-2). Default 1.

- ...:

  Additional arguments (unused).

## Value

Invisibly returns NULL for triad plots, or a ggplot2 object for
types/significance/patterns plots.

## See also

[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)
for the analysis that produces this object,
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
for statistical motif analysis

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
m <- extract_motifs(mat, significance = FALSE)
plot(m)

plot(m, type = "types")

```
