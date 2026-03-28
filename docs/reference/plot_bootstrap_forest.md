# Forest Plot for Bootstrap Network Results

A ggplot2-based forest plot for `net_bootstrap` and `boot_glasso`
objects. Each row is one network edge; horizontal bars span the
confidence interval and a filled square marks the point estimate. A
dashed reference line runs through zero.

Produces a ggplot2 forest plot where each row is one network edge, the
square marks the bootstrap mean estimate, and the horizontal bar spans
the selected interval. A dashed reference line runs through zero.
Significant edges are highlighted in colour; non-significant ones appear
in grey (only shown when `show_nonsig = TRUE`).

## Usage

``` r
plot_bootstrap_forest(x, ...)

# S3 method for class 'net_bootstrap'
plot_bootstrap_forest(
  x,
  alpha = NULL,
  layout = c("linear", "circular", "grouped"),
  interval = c("ci", "cr", "both"),
  show_nonsig = TRUE,
  sort_by = c("estimate", "significance", "name"),
  n_top = NULL,
  node_colors = NULL,
  sig_color = "#2C6E8A",
  cr_color = "#D4829A",
  nonsig_color = "#CCCCCC",
  ring_color = "#C8C8C8",
  median_color = "#AAAAAA",
  label_size = 2.9,
  label_color = NULL,
  point_size = if (match.arg(layout) == "circular") 2 else 3,
  r_inner = 0.38,
  r_outer = 0.72,
  gap_rad = 0.1,
  title = NULL,
  subtitle = NULL,
  ...
)

# S3 method for class 'boot_glasso'
plot_bootstrap_forest(
  x,
  alpha = NULL,
  layout = c("linear", "circular", "grouped"),
  interval = c("ci", "cr", "both"),
  show_nonsig = TRUE,
  sort_by = c("estimate", "significance", "name"),
  n_top = NULL,
  node_colors = NULL,
  sig_color = "#2C6E8A",
  cr_color = "#D4829A",
  nonsig_color = "#CCCCCC",
  ring_color = "#C8C8C8",
  median_color = "#AAAAAA",
  label_size = 2.9,
  label_color = NULL,
  point_size = if (match.arg(layout) == "circular") 2 else 3,
  r_inner = 0.38,
  r_outer = 0.72,
  gap_rad = 0.1,
  title = NULL,
  subtitle = NULL,
  ...
)

# S3 method for class 'net_bootstrap_group'
plot_bootstrap_forest(
  x,
  layout = c("linear", "circular"),
  interval = c("ci", "cr", "both"),
  show_nonsig = TRUE,
  n_top = NULL,
  all_edges = FALSE,
  pos_color = NULL,
  title = NULL,
  subtitle = NULL,
  label_size = 2.8,
  ...
)
```

## Arguments

- x:

  A `net_bootstrap` or `boot_glasso` object.

- ...:

  Currently unused.

- alpha:

  Significance threshold. Default: inherits from the object (`$ci_level`
  or `$alpha`), falling back to `0.05`.

- layout:

  `"linear"` (default) draws the classic tall forest plot; `"circular"`
  arranges each edge as a spoke around a circle, with the inner ring at
  the data minimum and the outer ring at the data maximum.

- interval:

  Which interval to display: `"ci"` (bootstrap confidence interval,
  default), `"cr"` (consistency range, stability inference only), or
  `"both"` (CI as outer bar, CR as inner bar).

- show_nonsig:

  Logical: include non-significant edges (greyed out)? Default `TRUE`.

- sort_by:

  How to order edges on the y-axis (linear layout) or clockwise from top
  (radial layout): `"estimate"` (default, ascending), `"significance"`
  (most significant at top), or `"name"` (alphabetical).

- n_top:

  Integer: restrict to the `n_top` edges with the largest absolute
  estimate. Applied after significance filtering. Default `NULL`.

- sig_color:

  Colour for significant CI bars and points. Default `"#2C6E8A"`
  (teal-blue).

- cr_color:

  Colour for the consistency range bar (`interval = "cr"` or `"both"`).
  Default `"#D4820A"` (amber).

- nonsig_color:

  Colour for non-significant edges. Default `"#CCCCCC"`.

- ring_color:

  Colour for the reference rings (radial layout only). Default
  `"#C8C8C8"`.

- median_color:

  Colour for the dashed median ring (radial layout only). Default
  `"#AAAAAA"`.

- label_size:

  Text size for edge labels (radial layout only). Default `2.3`.

- label_color:

  Fixed colour for edge labels (radial layout only). `NULL` (default)
  inherits the edge colour (teal for significant, grey for
  non-significant).

- point_size:

  Size of the estimate square. Default `3` (linear) or `2` (radial).

- title:

  Plot title. Default `NULL`.

- subtitle:

  Plot subtitle. Default `NULL`.

## Value

A `ggplot` object.

## Details

For `net_bootstrap` objects from stability inference, both a bootstrap
confidence interval (`ci_lower`/`ci_upper`) and a consistency range
(`cr_lower`/`cr_upper`) are available. Use `interval = "both"` to
overlay both on the same plot.
