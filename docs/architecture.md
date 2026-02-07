# cograph Architecture & Philosophy

> Modern Network Visualization for R

## Table of Contents
1. [Design Philosophy](#design-philosophy)
2. [Architecture Overview](#architecture-overview)
3. [Class System](#class-system)
4. [Input Pipeline](#input-pipeline)
5. [Layout System](#layout-system)
6. [Rendering Pipeline](#rendering-pipeline)
7. [Theming & Aesthetics](#theming--aesthetics)
8. [Specialized Features](#specialized-features)
9. [File Organization](#file-organization)

---

## Design Philosophy

### Core Principles

**1. Separation of Concerns**
cograph follows a clear pipeline architecture:
```
Input → Parse → Layout → Style → Render
```
Each stage is independent and can be customized without affecting others.

**2. Pipe-Friendly Immutable Transformations**
Every transformation function returns a new network object, enabling fluent chaining:
```r
cograph(data) |>
  sn_layout("spring") |>
  sn_theme("dark") |>
  sn_nodes(fill = "steelblue") |>
  splot()
```

**3. TNA-First Design**
Built with Transition Network Analysis as a primary use case:
- Auto-detection of `tna` and `group_tna` objects
- Preservation of TNA metadata (weights, inits, labels, data)
- Specialized TNA visualization functions

**4. Extensibility via Registries**
Global registries allow users to add custom:
- Layouts: `register_layout()`
- Shapes: `register_shape()`, `register_svg_shape()`
- Themes: `register_theme()`
- Palettes: Custom palette functions

**5. Reproducibility**
- Deterministic layouts via `seed` parameter
- All parameters stored in network object
- Consistent output across runs

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                        USER API                              │
│  as_cograph() │ cograph() │ splot() │ soplot() │ sn_*()    │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    S3 WRAPPER LAYER                          │
│                   cograph_network                            │
│  (User-facing interface with $ access to all components)     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     R6 CORE LAYER                            │
│  CographNetwork │ CographLayout │ CographTheme               │
│  (Internal mutable state and methods)                        │
└─────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┼───────────────────┐
          ▼                   ▼                   ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│  INPUT PARSERS  │ │ LAYOUT ENGINES  │ │    RENDERERS    │
│  parse_matrix() │ │ layout_spring() │ │ splot() - base  │
│  parse_tna()    │ │ layout_circle() │ │ soplot() - grid │
│  parse_igraph() │ │ layout_groups() │ │ sn_ggplot()     │
│  ...            │ │ ...             │ │                 │
└─────────────────┘ └─────────────────┘ └─────────────────┘
```

---

## Class System

### Dual-Class Pattern

cograph uses a dual-class pattern combining R6 (internal) with S3 (user-facing):

**Why this design?**
- **R6**: Efficient mutable state, encapsulation, methods
- **S3**: Idiomatic R interface, method dispatch, `$` access

### CographNetwork (R6)

Location: `R/class-network.R`

The core internal class storing all network data:

```r
CographNetwork <- R6::R6Class(
  private = list(
    .nodes = NULL,      # Data frame: id, label, x, y, ...
    .edges = NULL,      # Data frame: from, to, weight, ...
    .directed = FALSE,  # Boolean
    .weights = NULL,    # Numeric vector
    .layout = NULL,     # Computed coordinates
    .node_aes = NULL,   # Node aesthetics
    .edge_aes = NULL,   # Edge aesthetics
    .theme = NULL,      # CographTheme object
    .layout_info = NULL,# Layout metadata
    .plot_params = NULL # Stored parameters
  ),
  public = list(
    initialize = function(input, directed, node_labels),
    clone_network = function(),
    # Getters/setters for all private fields
    get_nodes = function(), set_nodes = function(nodes),
    get_edges = function(), set_edges = function(edges),
    # ... etc
  ),
  active = list(
    n_nodes = function(),
    n_edges = function(),
    is_directed = function(),
    has_weights = function()
  )
)
```

### cograph_network (S3 Wrapper)

Location: `R/class-network.R`

User-facing wrapper providing clean `$` access:

```r
# Structure
net <- list(
  # Edge data
  from = integer(),
  to = integer(),
  weight = numeric(),

  # Metadata
  nodes = data.frame(),
  directed = logical(),
  n_nodes = integer(),
  n_edges = integer(),
  labels = character(),
  source = character(),  # "matrix", "tna", "igraph", etc.

  # Optional
  layout = NULL,
  layout_info = NULL,
  tna = NULL,           # TNA integration
  node_groups = NULL    # For specialized plots
)
class(net) <- c("cograph_network", "list")
```

### CographLayout (R6)

Location: `R/class-layout.R`

Manages layout computation:
- Retrieves layout function from registry
- Computes coordinates
- Normalizes to [0,1] range with padding

### CographTheme (R6)

Location: `R/class-theme.R`

Visual theme parameters:
- background, node_fill, node_border
- edge_color, positive_color, negative_color
- label_color, label_size
- title_color, title_size

---

## Input Pipeline

### Universal Entry Point

`as_cograph(x)` accepts any supported format and returns a `cograph_network`:

```r
# All these work:
as_cograph(matrix)           # Adjacency/weight matrix
as_cograph(data.frame)       # Edge list
as_cograph(igraph_obj)       # igraph
as_cograph(network_obj)      # statnet
as_cograph(qgraph_obj)       # qgraph
as_cograph(tna_obj)          # tna
as_cograph(group_tna_obj)    # group_tna
```

### Auto-Detection Flow

Location: `R/input-parse.R`

```
parse_input(x)
    │
    ├─ is.matrix(x)?        → parse_matrix()
    ├─ is.data.frame(x)?    → parse_edgelist()
    ├─ inherits(x, "igraph")? → parse_igraph()
    ├─ inherits(x, "network")? → parse_statnet()
    ├─ inherits(x, "qgraph")? → parse_qgraph()
    ├─ inherits(x, "tna")?  → parse_tna()
    └─ inherits(x, "group_tna")? → parse_group_tna()
```

### Unified Output Format

All parsers return:
```r
list(
  nodes = data.frame(id, label, name, ...),
  edges = data.frame(from, to, weight, ...),
  directed = logical,
  weights = numeric,
  tna = NULL  # or TNA metadata if applicable
)
```

### TNA Integration

Location: `R/input-tna.R`

Special handling for TNA objects:
- Extracts `$weights`, `$inits`, `$labels`
- Stores original model in `$tna$model`
- Helper functions:
  - `is_tna_network(net)` - Check if TNA-based
  - `get_tna_model(net)` - Retrieve original tna object

---

## Layout System

### Built-in Layouts

| Layout | Description | File |
|--------|-------------|------|
| `"spring"` / `"fr"` | Fruchterman-Reingold force-directed | layout-spring.R |
| `"circle"` | Nodes arranged in circle | layout-circle.R |
| `"oval"` / `"ellipse"` | Nodes arranged in oval | layout-oval.R |
| `"groups"` | Grouped circular layout | layout-groups.R |
| `"grid"` | Regular grid | layout-registry.R |
| `"random"` | Random positions | layout-registry.R |
| `"star"` | Center node with ring | layout-registry.R |
| `"bipartite"` | Two-column layout | layout-registry.R |

### igraph Integration

Two-letter codes for igraph layouts:
- `"kk"` - Kamada-Kawai
- `"fr"` - Fruchterman-Reingold
- `"drl"` - DrL
- `"mds"` - MDS
- `"ni"` - Nicely
- `"tr"` - Tree/Reingold-Tilford

### Registry Pattern

```r
# Register custom layout
register_layout("my_layout", function(network, ...) {
  n <- nrow(network$get_nodes())
  data.frame(x = runif(n), y = runif(n))
})

# Use it
splot(net, layout = "my_layout")
```

---

## Rendering Pipeline

### Three Rendering Backends

| Function | Backend | Use Case |
|----------|---------|----------|
| `splot()` | Base R graphics | Fast, no dependencies, publication-ready |
| `soplot()` | Grid graphics | Better compositing, legends |
| `sn_ggplot()` | ggplot2 | Editable, combine with other ggplots |

### splot() Architecture

Location: `R/splot.R` (main) + supporting files

```
splot(x, ...)
    │
    ├─ Process parameters (splot-params.R)
    ├─ Setup plot device
    │
    ├─ Render edges (splot-edges.R)
    │   ├─ Calculate curves
    │   ├─ Draw splines
    │   ├─ Draw arrows (splot-arrows.R)
    │   └─ Draw edge labels (splot-labels.R)
    │
    ├─ Render nodes (splot-nodes.R)
    │   ├─ Basic shapes (circle, square, etc.)
    │   ├─ Donuts (proportional fill rings)
    │   ├─ Pies (segmented nodes)
    │   └─ Custom SVG shapes
    │
    └─ Render labels (splot-labels.R)
```

### Node Features

**Basic Shapes**: circle, square, triangle, diamond, pentagon, hexagon, star, heart, ellipse, cross

**Donuts**: Proportional fill rings
```r
splot(net, donut_fill = c(0.3, 0.7, 0.5))  # Per-node proportions
```

**Pies**: Segmented nodes
```r
splot(net, pie_values = list(c(0.4, 0.6), c(0.3, 0.3, 0.4)))
```

**Custom SVG**: Load any SVG shape
```r
splot(net, node_svg = "path/to/shape.svg")
```

### Edge Features

- **Curvature**: Bezier curves with tension control
- **Arrows**: Configurable size, angle, position
- **Auto-curving**: Reciprocal edges automatically curve apart
- **Self-loops**: Configurable rotation
- **Weight scaling**: Linear, log, sqrt, rank modes
- **Positive/negative coloring**: Auto-color by weight sign
- **CI visualization**: Uncertainty underlays
- **Edge labels**: With shadows, backgrounds

---

## Theming & Aesthetics

### Theme System

Location: `R/themes-builtin.R`, `R/themes-registry.R`

Built-in themes:
- `"classic"` (default): White background, blue nodes
- `"dark"`: Dark background, light elements
- `"minimal"`: Subtle styling
- `"colorblind"`: Optimized for color vision deficiency
- `"grayscale"`: Black and white only

```r
splot(net, theme = "dark")
```

### Palette System

Location: `R/palettes.R`

Built-in: viridis, colorblind, pastel, bright, grayscale

```r
cograph(net) |>
  sn_palette("viridis", target = "nodes", by = "degree") |>
  splot()
```

### Piping Functions

| Function | Purpose |
|----------|---------|
| `sn_layout()` | Change layout algorithm |
| `sn_theme()` | Apply visual theme |
| `sn_palette()` | Apply color palette |
| `sn_nodes()` | Customize node aesthetics |
| `sn_edges()` | Customize edge aesthetics |

---

## Specialized Features

### TNA Integration

```r
library(tna)
model <- tna(data)
net <- as_cograph(model)

# Check and retrieve
is_tna_network(net)      # TRUE
get_tna_model(net)       # Original tna object

# All TNA fields preserved
get_tna_model(net)$weights
get_tna_model(net)$inits
attr(get_tna_model(net)$data, "colors")
```

### Network Comparison

Location: `R/plot-compare.R`

```r
plot_compare(net1, net2)  # Visualize differences
# Green = positive (net1 > net2)
# Red = negative (net1 < net2)
```

### Multi-Group Plots

| Function | Purpose | File |
|----------|---------|------|
| `plot_htna()` | Heterogeneous/bipartite networks | plot-htna.R |
| `plot_mtna()` | Multi-cluster networks | plot-htna-multi.R |
| `plot_mlna()` | Multilevel 3D perspective | mlna.R |
| `plot_mcml()` | Multi-cluster multi-level | plot-mcml.R |

### Heatmaps

Location: `R/plot-heatmap.R`, `R/plot-ml-heatmap.R`

```r
plot_heatmap(net)         # Basic heatmap
plot_ml_heatmap(ml_net)   # Multilayer heatmap
```

---

## File Organization

```
R/
├── Core Architecture
│   ├── class-network.R      # CographNetwork R6 + S3 wrapper
│   ├── class-layout.R       # CographLayout R6
│   ├── class-theme.R        # CographTheme R6
│   ├── cograph.R            # Main API functions
│   └── aaa-globals.R        # Global registries
│
├── Input Parsing
│   ├── input-parse.R        # Dispatcher + helpers
│   ├── input-matrix.R       # Matrix parser
│   ├── input-edgelist.R     # Edge list parser
│   ├── input-igraph.R       # igraph parser
│   ├── input-statnet.R      # statnet parser
│   ├── input-qgraph.R       # qgraph parser
│   └── input-tna.R          # TNA parser + helpers
│
├── Layouts
│   ├── layout-registry.R    # Registry + built-ins
│   ├── layout-spring.R      # Force-directed
│   ├── layout-circle.R      # Circular
│   ├── layout-oval.R        # Oval/ellipse
│   └── layout-groups.R      # Grouped
│
├── Rendering - Base R
│   ├── splot.R              # Main function (2000+ lines)
│   ├── splot-nodes.R        # Node rendering
│   ├── splot-edges.R        # Edge rendering
│   ├── splot-arrows.R       # Arrow heads
│   ├── splot-labels.R       # Label placement
│   └── splot-geometry.R     # Geometric utilities
│
├── Rendering - Grid/ggplot
│   ├── render-grid.R        # Grid-based (soplot)
│   └── render-ggplot.R      # ggplot2 conversion
│
├── Aesthetics
│   ├── aes-nodes.R          # sn_nodes()
│   ├── aes-edges.R          # sn_edges()
│   ├── palettes.R           # Color palettes
│   ├── themes-builtin.R     # Built-in themes
│   └── themes-registry.R    # Theme registry
│
├── Shapes
│   ├── shapes-basic.R       # Basic shapes
│   ├── shapes-special.R     # Donuts, pies
│   ├── shapes-svg.R         # SVG support
│   └── shapes-registry.R    # Shape registry
│
├── Specialized Plots
│   ├── plot-compare.R       # Network comparison
│   ├── plot-heatmap.R       # Heatmaps
│   ├── plot-htna.R          # Heterogeneous
│   ├── plot-htna-multi.R    # Multi-cluster
│   ├── mlna.R               # Multilevel
│   └── plot-mcml.R          # Multi-cluster multilevel
│
└── Utilities
    ├── network-utils.R      # Network helpers
    ├── network-summary.R    # Summary statistics
    ├── centrality.R         # Centrality measures
    ├── output-save.R        # File saving
    └── utils-*.R            # Various utilities
```

---

## Example Workflows

### Basic Workflow
```r
library(cograph)

# Create from matrix
mat <- matrix(runif(25), 5, 5)
rownames(mat) <- colnames(mat) <- LETTERS[1:5]

cograph(mat) |>
  sn_layout("spring", seed = 42) |>
  sn_theme("classic") |>
  splot(title = "My Network")
```

### TNA Workflow
```r
library(cograph)
library(tna)

model <- tna(engagement_data)
net <- as_cograph(model)

# Use TNA features
splot(net,
  donut_fill = model$inits,  # Initial probabilities as donuts
  node_fill = attr(model$data, "colors"),  # TNA colors
  title = "Transition Network"
)

# Retrieve original for TNA analysis
original <- get_tna_model(net)
summary(original)
```

### Advanced Styling
```r
cograph(mat) |>
  sn_layout("circle") |>
  splot(
    node_shape = "hexagon",
    node_size = 0.08,
    donut_fill = c(0.3, 0.7, 0.5, 0.9, 0.4),
    donut_color = "steelblue",
    edge_width = "weight",
    edge_color = "weight",
    curvature = 0.3,
    theme = "dark"
  )
```
