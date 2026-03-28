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
  [`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)

- type:

  Plot type: "triads" (default network diagrams), "types" (bar plot),
  "significance" (z-score plot), or "patterns" (abstract MAN patterns)

- n:

  Number of triads to show. Default 20.

- colors:

  Colors for visualization. Default blue/red.

- res:

  Resolution for scaling (not used with grid graphics). Default 72.

- node_size:

  Size of nodes (1-10 scale, like splot). Default 5.

- label_size:

  Font size for node labels (3-letter abbreviations). Default 7.

- title_size:

  Font size for motif type title (e.g., "120C"). Default 7.

- stats_size:

  Font size for statistics text (n, z, p). Default 5.

- ncol:

  Number of columns in the plot grid. Default 5.

- legend:

  Logical, show abbreviation legend at bottom? Default TRUE.

- color:

  Color for nodes, edges, and labels. Default "#800020" (maroon).

- spacing:

  Spacing multiplier between cells (0.5-2). Default 1.

- ...:

  Additional arguments (unused)

## Value

Invisibly returns NULL for triad plots, or a ggplot2 object for other
types.

## See also

[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)
for the analysis that produces this object,
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md)
for statistical motif analysis

Other motifs:
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](http://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
if (FALSE) { # \dontrun{
Mod <- tna::tna(tna::group_regulation)
m <- extract_motifs(Mod, significance = TRUE)

# Default network diagram
plot(m)

# Customize appearance
plot(m, node_size = 0.15, label_size = 6, title_size = 9)

# Change layout
plot(m, ncol = 4, n = 12)

# Other plot types
plot(m, type = "types")
plot(m, type = "significance")
} # }
```
