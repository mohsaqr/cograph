# Render Legend for splot

Renders a comprehensive legend showing node groups, edge weight colors,
and optionally node sizes.

## Usage

``` r
render_legend_splot(
  groups,
  node_names,
  nodes,
  node_colors,
  position = "topright",
  cex = 0.8,
  show_edge_colors = FALSE,
  positive_color = "#2E7D32",
  negative_color = "#C62828",
  has_pos_edges = FALSE,
  has_neg_edges = FALSE,
  show_node_sizes = FALSE,
  node_size = NULL
)
```

## Arguments

- groups:

  Group assignments for nodes.

- node_names:

  Names for legend entries.

- nodes:

  Node data frame.

- node_colors:

  Vector of node colors.

- position:

  Legend position.

- cex:

  Text size.

- show_edge_colors:

  Logical: show positive/negative edge color legend?

- positive_color:

  Positive edge color.

- negative_color:

  Negative edge color.

- has_pos_edges:

  Logical: are there positive weighted edges?

- has_neg_edges:

  Logical: are there negative weighted edges?

- show_node_sizes:

  Logical: show node size legend?

- node_size:

  Vector of node sizes.
