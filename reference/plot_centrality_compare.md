# Plot Centrality Comparison

Plots one centrality measure across two or more groups as stacked,
faceted, grouped, dumbbell, line, or pyramid charts. The `"pyramid"`
style is a back-to-back horizontal bar chart for exactly two groups.
Groups are aligned on the node names they share.

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
  or network inputs. When every argument is named, the names are used as
  group labels if `group_labels` is NULL.

- measure:

  Character, a single centrality measure to compare. For network inputs,
  a name such as "strength" also matches a shared column with an
  "\_all", "\_in" or "\_out" suffix when the match is unique. For
  centrality data frames, it must be an exact column name. If NULL, the
  first shared measure is used.

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

  Colors of the nodes in `style = "facet"`. Either a named character
  vector mapping node name to color, an unnamed vector of colors applied
  in node order, or the name of a palette (`"cograph"`, `"okabe"`,
  `"viridis"`). NULL (default) uses the node colors stored in the first
  network when available and the cograph palette otherwise.

- sort_by:

  `"max"` (default) ranks nodes by highest value across groups;
  `"delta"` by range; `"first"` by first group; `"alpha"`
  alphabetically.

- top_n:

  Show top N nodes (by `sort_by`). Default: all.

- scale:

  `"raw"` (default, native values) or `"normalized"` (min-max scaled to
  \[0, 1\] within each group).

- show_values:

  Logical. Print the value of each bar or point. Default TRUE.

- size_by_value:

  Logical. For `"dumbbell"` style, scale dot size by centrality value.
  Default FALSE.

- size_range:

  Numeric vector of length 2 giving the min and max dot size (mm) when
  `size_by_value = TRUE`. Default `c(2, 9)`.

- orientation:

  Character: `"horizontal"` (default, nodes on y-axis) or `"vertical"`
  (nodes on x-axis). Ignored by the `"pyramid"` style.

- ncol:

  Number of facet columns for `style = "facet"`. Default NULL chooses
  automatically.

- title:

  Plot title. NULL (default) gives "Centrality comparison:" followed by
  the measure name.

- subtitle:

  Plot subtitle. When NULL, the `"pyramid"` style shows the two group
  labels and the other styles show none.

- centrality_args:

  Named list of additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  when inputs are networks.

## Value

A ggplot object.

## Examples

``` r
plot_centrality_compare(Full = regulation_net,
  Strong = threshold_edges(regulation_net, minimum = 0.1), measure = "strength")
```
