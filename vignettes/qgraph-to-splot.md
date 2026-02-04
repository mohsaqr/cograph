# Migrating from qgraph to splot

This guide helps users transition from `qgraph::qgraph()` to `Sonnet::splot()` for network visualization.

## Why Switch?

| Feature | qgraph | splot |
|---------|--------|-------|
| Active development | Limited | Active |
| Modern R practices | Legacy | snake_case API |
| TNA integration | Partial | Native |
| CI visualization | No | Built-in |
| Publication output | 300 DPI | 600 DPI default |
| Edge label templates | No | Yes (`{est}`, `{p}`, `{stars}`) |
| Donut charts | Basic | Advanced (shapes, double donuts) |

## Quick Start

```r
# Install Sonnet
# devtools::install_github("lamethods/Sonnet")

library(Sonnet)

# Basic usage - identical input format
adj <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)

# qgraph style:
# qgraph::qgraph(adj)

# Sonnet equivalent:
splot(adj)
```

## Parameter Mapping

### Node Parameters

| qgraph | splot | Notes |
|--------|-------|-------|
| `vsize` | `node_size` | Same scale |
| `vsize2` | `node_size2` | For ellipses |
| `shape` | `node_shape` | "circle", "square", "triangle", etc. |
| `color` | `node_fill` | Node fill color |
| `border.color` | `node_border_color` | |
| `border.width` | `node_border_width` | |
| `labels` | `labels` | TRUE, FALSE, or character vector |
| `label.cex` | `label_size` | |
| `label.color` | `label_color` | |

### Edge Parameters

| qgraph | splot | Notes |
|--------|-------|-------|
| `edge.color` | `edge_color` | |
| `edge.width` | `edge_width` | |
| `edge.labels` | `edge_labels` | TRUE shows weights |
| `edge.label.cex` | `edge_label_size` | |
| `edge.label.color` | `edge_label_color` | |
| `edge.label.bg` | `edge_label_bg` | Background color |
| `edge.label.position` | `edge_label_position` | 0-1 along edge |
| `lty` | `edge_style` | 1=solid, 2=dashed, 3=dotted |
| `curve` | `curvature` | Curve amount |
| `curveAll` | `curves = "force"` | Force all edges curved |
| `asize` | `arrow_size` | Arrow head size |
| `posCol` | `edge_positive_color` | Positive weight color |
| `negCol` | `edge_negative_color` | Negative weight color |
| `minimum` | `threshold` | Min weight to display |
| `maximum` | `maximum` | Max weight for scaling |
| `cut` | `edge_cutoff` | Two-tier scaling threshold |
| `directed` | `directed` | TRUE/FALSE/NULL (auto) |

### Pie/Donut Parameters

| qgraph | splot | Notes |
|--------|-------|-------|
| `pie` | `donut_fill` | Single value 0-1 per node |
| `pie` (list) | `pie_values` | Multiple segments per node |
| `pieColor` | `donut_color` | Fill color(s) |
| `pieBorder` | `pie_border_width` | |

### Layout & Display

| qgraph | splot | Notes |
|--------|-------|-------|
| `layout` | `layout` | "circle", "spring", "fr", "kk", etc. |
| `groups` | `groups` | For coloring/legend |
| `legend` | `legend` | TRUE/FALSE |
| `legend.cex` | `legend_size` | |
| `title` | `title` | Plot title |
| `title.cex` | `title_size` | |
| `mar` | `margins` | c(bottom, left, top, right) |
| `filetype` | `filetype` | "png", "pdf", "svg", etc. |
| `filename` | `filename` | Output file name |
| `width` | `width` | Output width (inches) |
| `height` | `height` | Output height (inches) |
| `repulsion` | (use igraph) | Spring layout uses igraph |

## Common Examples

### Basic Network

```r
# qgraph
qgraph(adj, layout = "circle", vsize = 10, edge.labels = TRUE)

# splot
splot(adj, layout = "circle", node_size = 10, edge_labels = TRUE)
```

### Weighted Network with Colors

```r
# qgraph
qgraph(adj, posCol = "darkgreen", negCol = "red", minimum = 0.1)

# splot
splot(adj, edge_positive_color = "darkgreen", edge_negative_color = "red", threshold = 0.1)
```

### Custom Node Appearance

```r
# qgraph
qgraph(adj,
       shape = "square",
       color = c("red", "blue", "green"),
       border.color = "black",
       border.width = 2,
       vsize = 15)

# splot
splot(adj,
      node_shape = "square",
      node_fill = c("red", "blue", "green"),
      node_border_color = "black",
      node_border_width = 2,
      node_size = 15)
```

### Curved Edges

```r
# qgraph
qgraph(adj, curve = 0.3, curveAll = TRUE)

# splot
splot(adj, curvature = 0.3, curves = "force")
```

### Pie/Donut Nodes (e.g., TNA initial probabilities)

```r
# qgraph style (single value per node = donut fill)
qgraph(adj, pie = c(0.2, 0.5, 0.3), pieColor = "steelblue")

# splot equivalent
splot(adj, donut_fill = c(0.2, 0.5, 0.3), donut_color = "steelblue")

# Multi-segment pie charts
splot(adj,
      pie_values = list(c(1, 2, 3), c(2, 2), c(1, 1, 1)),
      pie_colors = list(c("red", "blue", "green"), c("orange", "purple"), c("pink", "cyan", "yellow")))
```

### Save to File

```r
# qgraph
qgraph(adj, filetype = "png", filename = "network", width = 7, height = 7)

# splot (default 600 DPI for publication quality)
splot(adj, filetype = "png", filename = "network", width = 7, height = 7, res = 600)
```

## Features Unique to splot

### 1. Edge CI Underlays (Uncertainty Visualization)

```r
# Show confidence intervals as semi-transparent underlays
splot(adj,
      edge_ci = c(0.2, 0.5, 0.1, 0.3, 0.4, 0.2),  # CI widths
      edge_ci_scale = 2,      # Underlay thickness multiplier
      edge_ci_alpha = 0.15,   # Transparency
      edge_ci_style = 2)      # Dashed
```

### 2. Edge Label Templates

```r
# Show estimate with significance stars
splot(adj,
      edge_label_template = "{est}{stars}",
      edge_label_p = c(0.001, 0.02, 0.5, 0.008, 0.04, 0.001))

# Full statistical labels
splot(adj,
      edge_label_template = "{est} {range} {p}",
      edge_ci_lower = c(0.3, 0.1, 0.5, 0.2, 0.4, 0.1),
      edge_ci_upper = c(0.7, 0.5, 0.9, 0.6, 0.8, 0.5),
      edge_label_p = c(0.001, 0.02, 0.5, 0.008, 0.04, 0.001))
```

### 3. Edge Start Style (Direction Indicator)

```r
# Dotted line at edge start to show direction
splot(adj,
      edge_start_style = "dotted",
      edge_start_length = 0.2)  # 20% of edge
```

### 4. Polygon Donuts

```r
# Hexagonal donut nodes
splot(adj,
      node_shape = "hexagon",
      donut_fill = c(0.3, 0.7, 0.5),
      donut_shape = "hexagon")
```

### 5. Double Donuts

```r
# Two concentric rings per node
splot(adj,
      donut_fill = c(0.8, 0.6, 0.9),
      donut2_values = list(0.5, 0.3, 0.7),
      donut2_colors = list("orange", "purple", "cyan"))
```

## Converting Existing qgraph Objects

If you have existing qgraph code, use `from_qgraph()` for automatic conversion:

```r
library(qgraph)
q <- qgraph(adj, theme = "colorblind", posCol = "blue")

# Convert and plot with splot
from_qgraph(q)

# Or extract parameters for manual tweaking
params <- from_qgraph(q, plot = FALSE)
params$node_fill <- "coral"
do.call(splot, params)
```

## TNA Networks

For TNA (Transition Network Analysis), splot provides native support:

```r
library(tna)
trans <- tna(transition_matrix)

# Direct plotting (recommended)
splot(trans)

# Or use tplot() for qgraph-compatible parameter names
tplot(trans, vsize = 10, edge.labels = TRUE)
```

## Tips

1. **Layouts**: splot supports all igraph layouts via two-letter codes: `"kk"` (Kamada-Kawai), `"fr"` (Fruchterman-Reingold), `"drl"`, `"mds"`, etc.

2. **Themes**: Use `theme = "dark"`, `"minimal"`, `"colorblind"`, etc. for preset styling.

3. **Seed**: Set `seed = 42` (default) for reproducible layouts.

4. **Auto-detect direction**: Leave `directed = NULL` to auto-detect from matrix symmetry.

5. **Weight rounding**: By default, weights are rounded to 2 decimal places and near-zero edges removed. Control with `weight_digits`.

## Troubleshooting

### Plot looks different from qgraph

- Check `node_size` scaling - splot matches qgraph's scale
- Ensure `layout` is the same (qgraph defaults vary)
- Set `theme = NULL` to disable automatic theming

### Edges look faded

- Check `edge_alpha` (default 0.8)
- Check `edge_cutoff` - edges below this get faded
- Set `edge_cutoff = 0` to disable two-tier scaling

### Labels not showing

- Set `labels = TRUE` (nodes) or `edge_labels = TRUE` (edges)
- Check `label_size` / `edge_label_size` isn't too small
