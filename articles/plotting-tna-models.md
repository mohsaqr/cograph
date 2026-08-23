# TNA Plotting Gallery and Visual Smoke Test

This page is an executable visual smoke test for the complete plotting
surface of **tna** and its integration with **cograph**. Knitting stops
on the first plotting error. Statistical resampling uses deliberately
small iteration counts so that the page can be rebuilt during
development; the results below are for visual and API verification, not
inference.

The gallery covers:

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for single
  and grouped TNA models, bootstrap objects, centralities, cliques,
  communities, comparisons, permutations, reliability, sequence
  comparisons, and stability objects;
- [`hist()`](https://rdrr.io/r/graphics/hist.html),
  [`plot_model()`](http://sonsoles.me/tna/reference/plot_model.md),
  [`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md),
  [`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
  [`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md),
  [`plot_sequences()`](http://sonsoles.me/tna/reference/plot_sequences.md),
  and
  [`plot_associations()`](http://sonsoles.me/tna/reference/plot_associations.md);
- cograph’s enhanced TNA bootstrap, difference, permutation, forest, and
  heterogeneous-network renderers.

``` r

data.frame(
  package = c("tna", "cograph"),
  version = c(
    as.character(utils::packageVersion("tna")),
    as.character(utils::packageVersion("cograph"))
  )
)
#>   package version
#> 1     tna   1.2.3
#> 2 cograph   2.4.6
```

## Shared fixtures

The fixtures use 400 rows from `group_regulation`, split into two
groups. This is large enough to exercise real TNA objects while keeping
resampling fast.

## Core network plots

### Single TNA model

Exercises `plot.tna()` and its default cograph renderer.

``` r

plot(model, title = "Single TNA model", minimum = 0.05)
```

![](plotting-tna-models_files/figure-html/plot-tna-1.png)

### Grouped TNA models

Exercises `plot.group_tna()`.

``` r

plot(group_models, minimum = 0.05)
```

![](plotting-tna-models_files/figure-html/plot-group-tna-1.png)![](plotting-tna-models_files/figure-html/plot-group-tna-2.png)

### Heterogeneous TNA model

Passing a complete two-part `node_list` routes `plot.tna()` through
cograph’s heterogeneous TNA renderer.

``` r

split_at <- ceiling(length(model$labels) / 2)
node_list <- list(
  model$labels[seq_len(split_at)],
  model$labels[seq.int(split_at + 1, length(model$labels))]
)
plot(
  model,
  node_list = node_list,
  title = "Heterogeneous TNA model",
  minimum = 0.05
)
```

![](plotting-tna-models_files/figure-html/plot-heterogeneous-tna-1.png)

### Weight matrix

Exercises the exported
[`plot_model()`](http://sonsoles.me/tna/reference/plot_model.md) entry
point.

``` r

tna::plot_model(
  model$weights,
  labels = model$labels,
  title = "TNA weight matrix"
)
```

![](plotting-tna-models_files/figure-html/plot-model-matrix-1.png)

## Edge and state distributions

### Edge-weight histograms

Exercises both `hist.tna()` and `hist.group_tna()`.

``` r

hist(model, main = "Single-model edge weights")
```

![](plotting-tna-models_files/figure-html/hist-tna-1.png)

``` r

hist(group_models)
```

![](plotting-tna-models_files/figure-html/hist-group-tna-1.png)![](plotting-tna-models_files/figure-html/hist-group-tna-2.png)

### State frequencies

Exercises `plot_frequencies.tna()` and `plot_frequencies.group_tna()`.

``` r

tna::plot_frequencies(model)
```

![](plotting-tna-models_files/figure-html/plot-frequencies-tna-1.png)

``` r

tna::plot_frequencies(group_models)
```

![](plotting-tna-models_files/figure-html/plot-frequencies-group-1.png)

### Mosaic plots

Exercises mosaic methods for frequency TNA, grouped frequency TNA, and
prepared `tna_data` objects.

``` r

tna::plot_mosaic(frequency_model)
```

![](plotting-tna-models_files/figure-html/plot-mosaic-tna-1.png)

``` r

tna::plot_mosaic(group_frequency_models)
```

![](plotting-tna-models_files/figure-html/plot-mosaic-group-1.png)

``` r

tna::plot_mosaic(prepared_data, group = "cohort")
```

![](plotting-tna-models_files/figure-html/plot-mosaic-tna-data-1.png)

### Association network

Exercises `plot_associations.tna()` using the required integer-valued
frequency model.

``` r

tna::plot_associations(
  frequency_model,
  title = "Association network"
)
```

![](plotting-tna-models_files/figure-html/plot-associations-1.png)

## Sequence plots

### Single-model index and distribution plots

Exercises `plot_sequences.tna()` in both modes.

``` r

tna::plot_sequences(model, type = "index", tick = 1)
```

![](plotting-tna-models_files/figure-html/plot-sequences-tna-index-1.png)

``` r

tna::plot_sequences(model, type = "distribution", geom = "area", tick = 1)
```

![](plotting-tna-models_files/figure-html/plot-sequences-tna-distribution-1.png)

### Grouped sequences

Exercises `plot_sequences.group_tna()`.

``` r

tna::plot_sequences(group_models, type = "distribution", tick = 1)
```

![](plotting-tna-models_files/figure-html/plot-sequences-group-1.png)

### Data-frame sequences

Exercises `plot_sequences.default()` with an explicit grouping vector.

``` r

tna::plot_sequences(
  gallery_data,
  group = gallery_group,
  type = "distribution",
  tick = 1
)
```

![](plotting-tna-models_files/figure-html/plot-sequences-data-frame-1.png)

### Prepared TNA data

Exercises `plot_sequences.tna_data()` and metadata-based grouping.

``` r

tna::plot_sequences(
  prepared_data,
  group = "cohort",
  type = "index",
  tick = 1
)
```

![](plotting-tna-models_files/figure-html/plot-sequences-tna-data-1.png)

## Structural analysis plots

### Centralities

Exercises `plot.tna_centralities()` and `plot.group_tna_centralities()`.

``` r

plot(centrality_result, ncol = 3)
```

![](plotting-tna-models_files/figure-html/plot-centralities-1.png)

``` r

plot(group_centrality_result, ncol = 3)
```

![](plotting-tna-models_files/figure-html/plot-group-centralities-1.png)

### Communities

Exercises `plot.tna_communities()` and `plot.group_tna_communities()`.

``` r

plot(community_result, title = "Walktrap communities")
```

![](plotting-tna-models_files/figure-html/plot-communities-1.png)

``` r

plot(group_community_result)
```

![](plotting-tna-models_files/figure-html/plot-group-communities-1.png)![](plotting-tna-models_files/figure-html/plot-group-communities-2.png)

### Cliques

Exercises `plot.tna_cliques()` and `plot.group_tna_cliques()`. Only the
first dyad is drawn for each object to keep the gallery compact.

``` r

plot(clique_result, n = 1, ask = FALSE, title = "First dyad")
```

![](plotting-tna-models_files/figure-html/plot-cliques-1.png)

``` r

plot(group_clique_result, n = 1, ask = FALSE)
```

![](plotting-tna-models_files/figure-html/plot-group-cliques-1.png)![](plotting-tna-models_files/figure-html/plot-group-cliques-2.png)

## Bootstrap plots

### TNA bootstrap method

Exercises
[`plot.tna_bootstrap()`](https://sonsoles.me/cograph/reference/splot.tna_bootstrap.md).

``` r

plot(bootstrap_result, title = "TNA bootstrap")
```

![](plotting-tna-models_files/figure-html/plot-bootstrap-tna-1.png)

### Grouped bootstrap method

Exercises `plot.group_tna_bootstrap()`.

``` r

plot(group_bootstrap_result)
```

![](plotting-tna-models_files/figure-html/plot-bootstrap-group-1.png)![](plotting-tna-models_files/figure-html/plot-bootstrap-group-2.png)

### Cograph bootstrap render modes

These plots verify all enhanced
[`splot.tna_bootstrap()`](https://sonsoles.me/cograph/reference/splot.tna_bootstrap.md)
display modes.

``` r

cograph::splot(
  bootstrap_result,
  display = "significant",
  title = "Bootstrap: significant edges",
  show_stars = TRUE
)
```

![](plotting-tna-models_files/figure-html/splot-bootstrap-significant-1.png)

``` r

cograph::splot(
  bootstrap_result,
  display = "styled",
  title = "Bootstrap: styled full network",
  show_stars = TRUE
)
```

![](plotting-tna-models_files/figure-html/splot-bootstrap-styled-1.png)

``` r

cograph::splot(
  bootstrap_result,
  display = "ci",
  title = "Bootstrap: confidence intervals",
  show_ci = TRUE
)
```

![](plotting-tna-models_files/figure-html/splot-bootstrap-ci-1.png)

``` r

cograph::plot_bootstrap_forest(
  bootstrap_result,
  layout = "grouped",
  title = "Bootstrap forest grouped by source"
)
```

![](plotting-tna-models_files/figure-html/plot-bootstrap-forest-1.png)

## Permutation plots

### TNA permutation method

Exercises `plot.tna_permutation()`.

``` r

plot(permutation_result, title = "TNA permutation differences")
```

![](plotting-tna-models_files/figure-html/plot-permutation-tna-1.png)

### Grouped permutation method

Exercises `plot.group_tna_permutation()`.

``` r

plot(group_permutation_result)
```

![](plotting-tna-models_files/figure-html/plot-permutation-group-1.png)

### Cograph permutation renderer

Exercises the richer cograph permutation display directly.

``` r

cograph::plot_permutation(
  permutation_result,
  title = "Cograph permutation renderer",
  show_nonsig = TRUE
)
```

![](plotting-tna-models_files/figure-html/plot-permutation-cograph-1.png)

## Stability and reliability plots

### Centrality stability

Exercises `plot.tna_stability()`.

``` r

plot(stability_result)
```

![](plotting-tna-models_files/figure-html/plot-stability-1.png)

### Grouped centrality stability

Exercises `plot.group_tna_stability()`. The method returns its ggplots
invisibly, so the gallery prints each returned plot explicitly.

``` r

group_stability_plots <- plot(group_stability_result)
invisible(lapply(group_stability_plots, print))
```

![](plotting-tna-models_files/figure-html/plot-group-stability-1.png)![](plotting-tna-models_files/figure-html/plot-group-stability-2.png)

### Reliability distributions

Exercises all three modes of `plot.tna_reliability()`.

``` r

plot(reliability_result, type = "histogram")
```

![](plotting-tna-models_files/figure-html/plot-reliability-histogram-1.png)

``` r

plot(reliability_result, type = "density")
```

![](plotting-tna-models_files/figure-html/plot-reliability-density-1.png)

``` r

plot(reliability_result, type = "boxplot")
```

![](plotting-tna-models_files/figure-html/plot-reliability-boxplot-1.png)

## Comparison plots

### Difference networks

Exercises `plot_compare.tna()` and `plot_compare.group_tna()`, both of
which delegate to
[`cograph::plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md).

``` r

tna::plot_compare(
  model_first,
  model_second,
  title = "First half minus second half"
)
```

![](plotting-tna-models_files/figure-html/plot-compare-tna-1.png)

``` r

tna::plot_compare(group_models)
```

![](plotting-tna-models_files/figure-html/plot-compare-group-1.png)

### TNA comparison object

Exercises every mode of `plot.tna_comparison()`.

``` r

plot(comparison_result, type = "heatmap")
```

![](plotting-tna-models_files/figure-html/plot-comparison-heatmap-1.png)

``` r

plot(comparison_result, type = "scatterplot")
```

![](plotting-tna-models_files/figure-html/plot-comparison-scatterplot-1.png)

``` r

plot(comparison_result, type = "centrality_heatmap")
```

![](plotting-tna-models_files/figure-html/plot-comparison-centrality-heatmap-1.png)

``` r

plot(comparison_result, type = "weight_density")
```

![](plotting-tna-models_files/figure-html/plot-comparison-weight-density-1.png)

### Sequence comparison

Exercises `plot.tna_sequence_comparison()`.

``` r

plot(sequence_comparison_result, n = 10, cells = TRUE)
```

![](plotting-tna-models_files/figure-html/plot-sequence-comparison-1.png)

## Build result

If this section is visible, every plotting chunk above completed without
an error.

``` r

data.frame(
  status = "PASS",
  tna = as.character(utils::packageVersion("tna")),
  cograph = as.character(utils::packageVersion("cograph")),
  rendered_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
#>   status   tna cograph             rendered_at
#> 1   PASS 1.2.3   2.4.6 2026-08-23 16:40:03 UTC
```
