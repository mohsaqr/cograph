# Forest Plot for Bootstrap Network Results

A ggplot2-based forest plot for `net_bootstrap`, `net_bootstrap_group`,
`tna_bootstrap`, and `boot_glasso` objects. Each row is one network
edge; horizontal bars span the confidence interval and a filled square
marks the point estimate. A dashed reference line runs through zero.

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
  label_size = NULL,
  label_color = NULL,
  point_size = NULL,
  r_inner = NULL,
  r_outer = NULL,
  gap_rad = NULL,
  label_offset = NULL,
  src_label_size = NULL,
  margins = c(0.1, 0.1, 0.1, 0.1),
  scale = 1,
  title = NULL,
  subtitle = NULL,
  ...
)

# S3 method for class 'tna_bootstrap'
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
  label_size = NULL,
  label_color = NULL,
  point_size = NULL,
  r_inner = NULL,
  r_outer = NULL,
  gap_rad = NULL,
  label_offset = NULL,
  src_label_size = NULL,
  margins = c(0.1, 0.1, 0.1, 0.1),
  scale = 1,
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
  label_size = NULL,
  label_color = NULL,
  point_size = NULL,
  r_inner = NULL,
  r_outer = NULL,
  gap_rad = NULL,
  label_offset = NULL,
  src_label_size = NULL,
  margins = c(0.1, 0.1, 0.1, 0.1),
  scale = 1,
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

  A `tna_bootstrap` (from
  [`tna::bootstrap`](http://sonsoles.me/tna/reference/bootstrap.md)),
  `net_bootstrap`, `net_bootstrap_group`, or `boot_glasso` object.

- ...:

  Currently unused.

- alpha:

  Significance threshold. Default: inherits from the object (`$ci_level`
  or `$alpha`), falling back to `0.05`.

- layout:

  `"linear"` (default) draws the classic tall forest plot; `"circular"`
  arranges each edge as a spoke around a circle, with the inner ring at
  the data minimum and the outer ring at the data maximum; `"grouped"`
  arranges edges in sectors by source node where supported.

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

- node_colors:

  Optional node-colour vector for grouped radial layouts.

- sig_color:

  Colour for significant CI bars and points. Default `"#2C6E8A"`
  (teal-blue).

- cr_color:

  Colour for the consistency range bar (`interval = "cr"` or `"both"`).
  Default `"#D4829A"`.

- nonsig_color:

  Colour for non-significant edges. Default `"#CCCCCC"`.

- ring_color:

  Colour for the reference rings (radial layout only). Default
  `"#C8C8C8"`.

- median_color:

  Colour for the dashed median ring (radial layout only). Default
  `"#AAAAAA"`.

- label_size:

  Text size for edge labels (radial and grouped layouts). Default `NULL`
  for automatic sizing in the main methods, or `2.8` for
  `net_bootstrap_group`.

- label_color:

  Fixed colour for edge labels (radial layout only). `NULL` (default)
  inherits the edge colour (teal for significant, grey for
  non-significant).

- point_size:

  Size of the estimate square. Default `3` (linear) or `2` (radial).

- r_inner:

  Inner ring radius (grouped layout). Default `NULL` (auto).

- r_outer:

  Outer ring radius (grouped layout). Default `NULL` (auto).

- gap_rad:

  Gap in radians between sectors (grouped layout). Default `NULL`
  (auto).

- label_offset:

  Distance between outer ring and labels (grouped layout). Default
  `NULL` (auto).

- src_label_size:

  Text size for source node labels in the center (grouped layout).
  Default `NULL` (auto, `label_size * 0.80`).

- margins:

  Margins as `c(bottom, left, top, right)` fractions (grouped layout).
  Default `c(0.1, 0.1, 0.1, 0.1)`.

- scale:

  Scaling factor applied to all text and point sizes (grouped layout).
  Default `1`. Use values \> 1 for high-DPI output, \< 1 for small
  devices.

- title:

  Plot title. Default `NULL`.

- subtitle:

  Plot subtitle. Default `NULL`.

- all_edges:

  For `net_bootstrap_group`, show the union of group edges instead of
  only edges common to all groups. Default `FALSE`.

- pos_color:

  Currently unused by the `net_bootstrap_group` method.

## Value

A `ggplot` object.

## Details

For `net_bootstrap` objects from stability inference, both a bootstrap
confidence interval (`ci_lower`/`ci_upper`) and a consistency range
(`cr_lower`/`cr_upper`) are available. Use `interval = "both"` to
overlay both on the same plot.

## Examples

``` r
# Bootstrap a TNA built from sequence data (required by tna::bootstrap)
d    <- tna::prepare_data(
  tna::group_regulation_long,
  actor = "Actor", time = "Time", action = "Action"
)
#> ── Preparing Data ──────────────────────────────────────────────────────────────
#> ℹ Input data dimensions: 27533 rows, 6 columns
#> ℹ First few time values: 2025-01-01 08:27:07.712698, 2025-01-01
#>   08:35:20.712698, and 2025-01-01 08:42:18.712698
#> ℹ Number of values to parse: 27533
#> ℹ Sample values: 2025-01-01 08:27:07.712698, 2025-01-01 08:35:20.712698, and
#>   2025-01-01 08:42:18.712698
#> ℹ Sample of parsed times: 2025-01-01 08:27:07.712698, 2025-01-01
#>   08:35:20.712698, and 2025-01-01 08:42:18.712698
#> ℹ Time threshold for new session: 900 seconds
#> ℹ Total number of sessions: 2000
#> ℹ Number of unique users: 2000
#> ℹ Total number of actions: 27533
#> ℹ Maximum sequence length: 26 actions
#> ℹ Time range: 2025-01-01 08:01:16.009382 to 2025-01-01 13:03:20.238288
Mod  <- tna::tna(d)
boot <- tna::bootstrap(Mod, iter = 50)
plot_bootstrap_forest(boot, n_top = 8)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_vline()`).
```
