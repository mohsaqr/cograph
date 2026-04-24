# Plot Centrality Comparison (Pyramid)

Back-to-back horizontal bar chart comparing a centrality measure across
two groups. Each row is a node; left bar is group 1, right bar is group
2. Bar fill is per-node (identity) – either inherited from the network's
node colors or supplied via `node_colors`. Visual delta is communicated
by bar length alone.

## Usage

``` r
plot_centrality_compare(
  ...,
  measure = NULL,
  style = c("stacked", "facet", "grouped", "dumbbell", "line", "pyramid"),
  group_labels = NULL,
  group_colors = NULL,
  node_colors = NULL,
  sort_by = c("max", "delta", "first", "alpha"),
  top_n = NULL,
  scale = c("raw", "normalized"),
  show_values = TRUE,
  size_by_value = FALSE,
  size_range = c(2, 9),
  orientation = c("horizontal", "vertical"),
  ncol = NULL,
  title = NULL,
  subtitle = NULL,
  centrality_args = list()
)
```

## Arguments

- ...:

  Two or more centrality data frames (from
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md))
  or network inputs. Names are used as group labels when `group_labels`
  is NULL.

- measure:

  Character, a single centrality measure to compare. If NULL, the first
  shared measure is used.

- style:

  Character: `"stacked"` (default), `"facet"`, `"grouped"`,
  `"dumbbell"`, `"line"`, or `"pyramid"` (2 groups only).

- group_labels:

  Character vector with one label per group. Default
  `c("Group 1", "Group 2", ...)`.

- group_colors:

  Character vector of colors, one per group. Default cycles through the
  cograph palette.

- node_colors:

  Optional. Either a named character vector mapping node name to color,
  an unnamed vector of colors applied in node order, or the name of a
  palette (`"cograph"`, `"okabe"`, `"viridis"`). Used by
  `style = "facet"`.

- sort_by:

  `"max"` (default) ranks nodes by highest value across groups;
  `"delta"` by range; `"first"` by first group; `"alpha"`
  alphabetically.

- top_n:

  Show top N nodes (by `sort_by`). Default: all.

- scale:

  `"raw"` (default, native values on each side) or `"normalized"` (\[0,
  1\] within each side before plotting).

- show_values:

  Logical. Print the value inside each bar. Default TRUE.

- size_by_value:

  Logical. For `"dumbbell"` style, scale dot size by centrality value.
  Default FALSE.

- size_range:

  Numeric vector of length 2 giving the min and max dot size (mm) when
  `size_by_value = TRUE`. Default `c(2, 9)`.

- orientation:

  Character: `"horizontal"` (default, nodes on y-axis) or `"vertical"`
  (nodes on x-axis).

- ncol:

  Number of facet columns for `style = "facet"`. Default NULL chooses
  automatically.

- title:

  Plot title.

- subtitle:

  Plot subtitle. Auto-generated when NULL.

- centrality_args:

  Named list of additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  when inputs are networks.

## Value

A ggplot object.

## Examples

``` r
set.seed(1)
m1 <- matrix(runif(25), 5, 5); diag(m1) <- 0
m2 <- matrix(runif(25), 5, 5); diag(m2) <- 0
rownames(m1) <- colnames(m1) <- LETTERS[1:5]
rownames(m2) <- colnames(m2) <- LETTERS[1:5]
plot_centrality_compare(m1, m2, measure = "strength",
                        group_labels = c("Pre", "Post"))
```
