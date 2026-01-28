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
