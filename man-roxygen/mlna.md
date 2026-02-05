# Multilevel Network Analysis (mlna)

Visualize multilevel/multiplex networks where multiple layers are stacked in a 3D perspective view.

---

## Overview

The `mlna()` / `plot_mlna()` function creates a pseudo-3D visualization of multilevel networks:

- Each **layer** contains nodes connected by **solid edges** (within-layer)
- **Dashed lines** connect nodes between adjacent layers (inter-layer edges)
- Each layer is enclosed in a **parallelogram shell** giving a 3D appearance
- Layers are displayed from **top to bottom** in list order

---

## Basic Syntax

```r
mlna(model, layer_list, ...)
plot_mlna(model, layer_list, ...)  # same function
```

---

## Arguments

### Required Arguments

| Argument | Description |
|----------|-------------|
| `model` | A tna object or weight matrix |
| `layer_list` | Named list of character vectors defining layers |

### Layout Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `layout` | `"horizontal"` | Node layout: `"horizontal"`, `"circle"`, `"spring"` |
| `layer_spacing` | `2.2` | Vertical distance between layer centers |
| `layer_width` | `4.5` | Horizontal width of each layer shell |
| `layer_depth` | `2.2` | Depth of each layer (for 3D effect) |
| `skew_angle` | `25` | Angle of perspective skew in degrees |
| `node_spacing` | `0.7` | Node placement ratio within layer (0-1) |

### Appearance Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `colors` | `NULL` | Vector of colors for each layer (auto-generated if NULL) |
| `shapes` | `NULL` | Vector of shapes for each layer |
| `edge_colors` | `NULL` | Vector of edge colors by source layer |
| `node_size` | `3` | Size of nodes |
| `curvature` | `0.15` | Edge curvature for within-layer edges |

### Edge Control Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `within_edges` | `TRUE` | Show edges within layers (solid lines) |
| `between_edges` | `TRUE` | Show edges between adjacent layers (dashed) |
| `between_style` | `2` | Line style for between-layer edges (1=solid, 2=dashed, 3=dotted) |
| `minimum` | `0` | Minimum edge weight threshold |

### Display Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `show_border` | `TRUE` | Draw parallelogram shells around layers |
| `legend` | `TRUE` | Show legend |
| `legend_position` | `"topright"` | Position for legend |

---

## Layer List Format

The `layer_list` must be a **named list** of character vectors:

```r
layer_list <- list(
  LayerName1 = c("node1", "node2", "node3"),
  LayerName2 = c("node4", "node5", "node6"),
  LayerName3 = c("node7", "node8", "node9")
)
```

### Rules

- Minimum 2 layers required
- Node names must match the matrix row/column names
- **No overlap**: Each node can only belong to one layer
- Layers are drawn **top to bottom** in list order

---

## Layout Options

### `"horizontal"` (default)

Spreads nodes horizontally within each layer.

```r
mlna(m, layers, layout = "horizontal")
```

### `"circle"`

Arranges nodes in an ellipse within each layer.

```r
mlna(m, layers, layout = "circle")
```

### `"spring"`

Uses force-directed placement based on within-layer connections.

```r
mlna(m, layers, layout = "spring")
```

---

## Available Shapes

Shapes cycle through layers automatically:

| Shape | Description |
|-------|-------------|
| `"circle"` | Filled circle |
| `"square"` | Filled square |
| `"diamond"` | Diamond shape |
| `"triangle"` | Triangle pointing up |
| `"pentagon"` | Pentagon |
| `"hexagon"` | Hexagon |
| `"star"` | Star shape |
| `"cross"` | Cross/plus |

---

## Default Color Palettes

### Layer Colors (fills)

```r
c("#ffd89d", "#a68ba5", "#7eb5d6", "#98d4a2",
  "#f4a582", "#92c5de", "#d6c1de", "#b8e186",
  "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9")
```

### Edge Colors

```r
c("#e6a500", "#7a5a7a", "#4a90b8", "#5cb85c",
  "#d9534f", "#5bc0de", "#9b59b6", "#8bc34a",
  "#ff7043", "#78909c", "#ab47bc", "#aed581")
```

---

## Examples

### Basic Usage

```r
# Create sample data
set.seed(42)
nodes <- paste0("N", 1:15)
m <- matrix(runif(225, 0, 0.3), 15, 15)
diag(m) <- 0
colnames(m) <- rownames(m) <- nodes

# Define 3 layers
layers <- list(
  Macro = paste0("N", 1:5),
  Meso  = paste0("N", 6:10),
  Micro = paste0("N", 11:15)
)

# Basic plot
mlna(m, layers)
```

### Customized Appearance

```r
mlna(m, layers,
     layout = "circle",
     layer_spacing = 2.5,
     layer_width = 5,
     layer_depth = 2.5,
     skew_angle = 30,
     node_size = 4,
     curvature = 0.2)
```

### Custom Colors

```r
mlna(m, layers,
     colors = c("coral", "steelblue", "forestgreen"),
     edge_colors = c("darkred", "darkblue", "darkgreen"))
```

### Edge Filtering

```r
# Only show edges with weight > 0.1
mlna(m, layers, minimum = 0.1)

# Hide between-layer edges
mlna(m, layers, between_edges = FALSE)

# Hide within-layer edges
mlna(m, layers, within_edges = FALSE)

# Change between-layer edge style to dotted
mlna(m, layers, between_style = 3)
```

### Minimal Display

```r
mlna(m, layers,
     show_border = FALSE,
     legend = FALSE)
```

### Spring Layout for Dense Networks

```r
mlna(m, layers,
     layout = "spring",
     curvature = 0.1,
     minimum = 0.15)
```

---

## Typical Use Cases

### 1. Organizational Hierarchy

```r
layers <- list(
  Executive  = c("CEO", "CFO", "CTO"),
  Management = c("Manager1", "Manager2", "Manager3", "Manager4"),
  Staff      = c("Staff1", "Staff2", "Staff3", "Staff4", "Staff5")
)
mlna(communication_matrix, layers)
```

### 2. Ecological Food Web

```r
layers <- list(
  Predators  = c("Eagle", "Wolf", "Bear"),
  Herbivores = c("Deer", "Rabbit", "Mouse"),
  Plants     = c("Grass", "Shrub", "Tree")
)
mlna(food_web, layers)
```

### 3. Social Network Levels

```r
layers <- list(
  Macro = c("Institution1", "Institution2"),
  Meso  = c("Group1", "Group2", "Group3"),
  Micro = c("Person1", "Person2", "Person3", "Person4")
)
mlna(social_network, layers)
```

### 4. Learning Analytics

```r
layers <- list(
  Cognitive    = c("Remember", "Understand", "Apply"),
  Behavioral   = c("Engage", "Practice", "Perform"),
  Metacogntic  = c("Plan", "Monitor", "Reflect")
)
mlna(learning_transitions, layers)
```

---

## Tips

1. **Layer Order**: Layers are drawn top-to-bottom, so put the most important/highest level first

2. **Minimum Threshold**: Use `minimum` to filter weak edges and reduce visual clutter

3. **Node Spacing**: Adjust `node_spacing` (0-1) to control how spread out nodes are within layers

4. **Perspective**: Increase `skew_angle` for more dramatic 3D effect, decrease for flatter view

5. **Spring Layout**: Best for networks where within-layer connections are meaningful

6. **Custom Colors**: Match layer colors to your domain (e.g., organizational colors)

---

## Return Value

Returns `NULL` invisibly. The function is called for its side effect of producing a plot.

---

## See Also

- `splot()` - Standard network plotting
- `plot_mtna()` / `mtna()` - Multi-panel TNA plotting
- `plot_htna()` - Hierarchical TNA plotting
