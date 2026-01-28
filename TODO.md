# Sonnet TODO

## Pending Features

### Add Metadata Storage to SonnetNetwork

**Goal:** Add metadata storage capability to SonnetNetwork, allowing users to store and retrieve custom attributes for the graph, nodes, and edges - similar to igraph's attribute system.

**Changes needed in `R/class-network.R`:**

1. Add private field:
   ```r
   .metadata = list()
   ```

2. Add graph-level metadata methods:
   - `set_metadata(key, value)`
   - `get_metadata(key = NULL)`
   - `set_metadata_all(metadata)` - for cloning

3. Add node attribute methods:
   - `set_node_attr(name, value, index = NULL)`
   - `get_node_attr(name, index = NULL)`
   - `list_node_attrs()` - returns non-core column names

4. Add edge attribute methods:
   - `set_edge_attr(name, value, index = NULL)`
   - `get_edge_attr(name, index = NULL)`
   - `list_edge_attrs()` - returns non-core column names

5. Update `clone_network()` to copy metadata

6. Update `as_sonnet_network()` to expose `obj$metadata`

**Changes needed in `R/sonnet.R`:**

Add pipe-friendly convenience functions:
- `sn_set_meta(network, key, value)`
- `sn_get_meta(network, key = NULL)`

**Usage examples:**
```r
# Graph-level metadata
net <- sonnet(adj) |>
  sn_set_meta("title", "My Network") |>
  sn_set_meta("created", Sys.Date())

sn_get_meta(net, "title")  # "My Network"

# Node attributes
net$network$set_node_attr("type", c("A", "B", "A"))
net$network$get_node_attr("type")
net$network$list_node_attrs()

# Edge attributes
net$network$set_edge_attr("confidence", c(0.9, 0.8, 0.7))
net$network$list_edge_attrs()
```

---

### Meta-Network Visualization (Network of Networks)

**Goal:** Visualize multiple networks as interconnected "super-nodes", where each super-node is a complete network rendered in a circular layout, and edges connect between these network-nodes.

**Use cases:**
- Multi-layer/multiplex networks
- Temporal network comparison (networks at different time points)
- Community visualization (each community as a mini-network)
- Hierarchical/nested network structures
- Cross-group relationships

**Proposed API:**
```r
# List of networks (each becomes a "super-node")
networks <- list(
  group_a = adj_matrix_a,
  group_b = adj_matrix_b,
  group_c = adj_matrix_c
)

# Inter-network connections (which networks connect to which)
meta_edges <- data.frame(
  from = c("group_a", "group_a", "group_b"),
  to = c("group_b", "group_c", "group_c"),
  weight = c(0.8, 0.5, 0.6)
)

# Plot the meta-network
splot_meta(
  networks = networks,
  meta_edges = meta_edges,
  outer_layout = "circle",      # Layout of network-nodes
  inner_layout = "circle",      # Layout within each network
  network_size = "auto",        # Size of each network circle (or by n_nodes)
  network_colors = NULL,        # Fill colors for network backgrounds
  meta_edge_style = "curved",   # Style for inter-network edges
  show_network_labels = TRUE,   # Show network names
  show_node_labels = FALSE      # Show individual node labels
)
```

**Visual concept:**
```
        ┌─────────┐
       ╱  Net A    ╲
      │  ●──●──●   │
       ╲   ╲ ╱    ╱
        └────┼────┘
             │
    ╭────────┴────────╮
    │                 │
┌───┴───┐         ┌───┴───┐
│ Net B │─────────│ Net C │
│ ●─●─● │         │ ●─●─● │
└───────┘         └───────┘
```

**Implementation approach:**
1. Compute outer layout positions for each network
2. For each network, compute inner layout and scale to fit within a circle
3. Draw network backgrounds (optional circles/boundaries)
4. Draw all inner network edges
5. Draw all inner network nodes
6. Draw meta-edges connecting between networks
7. Draw labels

**Variations:**
- `splot_nested()` - For hierarchical nesting (networks within networks)
- `splot_layers()` - For multi-layer networks stacked vertically
- `splot_temporal()` - For time-series of networks with transitions

---

### Multi-Layer Stacked Network Visualization

**Goal:** Visualize multiplex/multi-layer networks as stacked rectangular planes with inter-layer edges, creating a 2.5D perspective view.

**Use cases:**
- Multiplex networks (same nodes, different edge types per layer)
- Temporal networks (network snapshots at different time points)
- Multi-relational data (e.g., friendship + work + family networks)
- Brain connectivity across frequency bands
- Multi-modal transportation networks

**Visual concept:**
```
    Layer 3 (t=3)
    ┌─────────────────┐
    │  ●───●     ●    │
    │  │╲  │    ╱│    │
    │  ● ╲─●───● │    │
    └──┼──┼───┼──┼────┘
       │  │   │  │
    Layer 2 (t=2)
    ┌──┼──┼───┼──┼────┐
    │  ●──●───●──●    │
    │   ╲ │   │ ╱     │
    │    ╲●───●╱      │
    └─────┼───┼───────┘
          │   │
    Layer 1 (t=1)
    ┌─────┼───┼───────┐
    │  ●──●   ●──●    │
    │  │   ╲ ╱   │    │
    │  ●────●────●    │
    └─────────────────┘
```

**Proposed API:**
```r
# Option 1: List of adjacency matrices (same nodes across layers)
layers <- list(
  "Time 1" = adj_t1,
  "Time 2" = adj_t2,
  "Time 3" = adj_t3
)

# Inter-layer edges (optional - connects node i in layer j to node k in layer l)
inter_edges <- data.frame(
  from_layer = c(1, 1, 2),
  from_node = c(1, 3, 2),
  to_layer = c(2, 2, 3),
  to_node = c(1, 3, 2),  # Often same node across layers
  weight = c(1, 1, 1)
)

splot_multilayer(
  layers = layers,
  inter_edges = inter_edges,       # NULL = auto-connect same nodes
  layout = "spring",               # Layout algorithm (shared or per-layer
  shared_layout = TRUE,            # Use same node positions across layers
  layer_spacing = 0.3,             # Vertical spacing between layers
  perspective = 0.15,              # 3D perspective amount (0 = flat)
  tilt = 15,                       # Tilt angle in degrees
  layer_fill = c("#E3F2FD", "#FFF3E0", "#E8F5E9"),  # Layer background colors
  layer_alpha = 0.3,               # Layer background transparency
  layer_border = TRUE,             # Draw layer borders
  inter_edge_color = "gray50",     # Color for inter-layer edges
  inter_edge_style = "dashed",     # Style for inter-layer edges
  inter_edge_alpha = 0.5,          # Transparency for inter-layer edges
  show_layer_labels = TRUE,        # Show layer names
  label_position = "left"          # Where to place layer labels
)
```

**Auto-connect modes for inter-layer edges:**
```r
# Connect same nodes across adjacent layers
splot_multilayer(layers, inter_edges = "adjacent")

# Connect same nodes across all layers
splot_multilayer(layers, inter_edges = "all")

# No inter-layer edges
splot_multilayer(layers, inter_edges = NULL)

# Custom inter-layer edges
splot_multilayer(layers, inter_edges = my_inter_edges_df)
```

**Additional features:**
- `highlight_node_across_layers(node_id)` - Highlight a node and its inter-layer connections
- `layer_opacity` - Fade distant layers for depth perception
- `rotate_view(angle)` - Rotate the 3D perspective
- Side-by-side mode as alternative to stacked
- Animation to "flip through" layers

**Implementation approach:**
1. Compute base layout (shared or per-layer)
2. Apply perspective transformation to each layer's coordinates
3. Add vertical offset for layer stacking
4. Draw layers back-to-front (painter's algorithm):
   - Layer background/border
   - Intra-layer edges
   - Inter-layer edges (from current to lower layers)
   - Nodes
   - Labels
5. Handle occlusion with transparency or smart ordering

---

### Export/Import Functions

- `sn_export(net, "network.graphml")` - Export to GraphML, GML, DOT formats
- `sn_save_rds(net, "network.rds")` / `sn_load_rds("network.rds")` - Save/load Sonnet networks
- `sn_to_json(net)` / `sn_from_json()` - JSON serialization

---

### Interactive Visualization

- `sn_interactive(net)` - Interactive plot with plotly/htmlwidgets
- Node hover tooltips showing attributes
- Click to select/highlight nodes
- Zoom and pan controls

---

### Additional Layouts

- Sugiyama layout for DAGs
- Hierarchical/tree layout improvements
- Radial layout
- Arc diagram layout
- Bipartite layout improvements

---

### Network Operations

- `sn_subset(net, nodes)` - Extract subnetwork
- `sn_filter_nodes(net, condition)` - Filter by node attribute
- `sn_filter_edges(net, condition)` - Filter by edge weight/attribute
- `sn_merge(net1, net2)` - Combine networks
- `sn_reverse(net)` - Reverse edge directions

---

### Analysis Integration

- `sn_color_by_community(net)` - Auto-detect and color communities
- `sn_size_by_centrality(net, type)` - Size nodes by degree/betweenness/etc.
- `sn_highlight_path(net, from, to)` - Highlight shortest path
- `sn_highlight_neighbors(net, node)` - Highlight node neighborhood

---

### Visual Enhancements

- Edge bundling for dense networks
- Curved edge improvements (Bezier curves)
- Node images/icons
- Custom node shapes from SVG
- Gradient edge colors
- Edge labels with automatic placement to avoid overlap

---

### Legend and Annotation

- Improved automatic legend generation
- `sn_annotate(net, x, y, text)` - Add text annotations
- `sn_add_title(net, title, subtitle)` - Structured titles
- Scale bars for weighted edges

---

### Animation

- `sn_animate(net1, net2, frames)` - Animate between layouts
- `sn_animate_growth(net)` - Show network growing over time
- GIF/video export for animations
