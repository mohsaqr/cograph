# Sonnet 1.5.1

## Breaking Changes (with backwards compatibility)

The following parameters have been renamed for consistency. The old names still work but emit deprecation warnings:

| Old Name | New Name | Reason |
|----------|----------|--------|
| `esize` | `edge_size` | Add `edge_` prefix, expand abbreviation |
| `cut` | `edge_cutoff` | Add `edge_` prefix, clarify meaning |
| `usePCH` | `use_pch` | Fix camelCase to snake_case |
| `positive_color` | `edge_positive_color` | Add `edge_` prefix (matches theme storage) |
| `negative_color` | `edge_negative_color` | Add `edge_` prefix (matches theme storage) |
| `donut_border_lty` | `donut_line_type` | Expand `lty` abbreviation |

## Improvements

- `edge_label_fontface` now accepts string values ("plain", "bold", "italic", "bold.italic") in addition to numeric values, matching other fontface parameters
- Added deprecation helper infrastructure for backwards-compatible parameter renaming
- Standardized all public API parameters to use consistent snake_case naming

# Sonnet 1.4.0

## New Features

- Added `mlna()` for multilevel network visualization with 3D perspective
- Added `mtna()` for multi-cluster network visualization
- Added `plot_htna()` for hierarchical multi-group polygon network layouts with circular layout option
- Added `tplot()` as qgraph drop-in replacement
- Added `arrow_angle` parameter for customizable arrowheads

## Bug Fixes

- Fixed Rd cross-reference warning in splot documentation
- Fixed pie/donut segment divider lines when border width is 0

# Sonnet 1.3.1

## New Features

- Added `edge_start_dot_density` parameter for TNA-style dotted edges
- Added direct support for tna objects via `from_tna()`
- Added direct support for statnet network and qgraph objects
- Added auto-conversion of `pie_values` vector to `donut_fill` when values are in [0,1]
- Removed underscore prefix requirement for custom SVG shapes

## Bug Fixes

- Fixed TNA defaults being overwritten in splot()
- Fixed self-loop edge labels and adjusted dotted pattern spacing
- Fixed donut_shape validation error with custom SVG shapes
- Fixed title clipping issue
- Fixed edge rendering crash
- Fixed R CMD check errors and warnings

## Improvements

- Added strict vectorization for node/edge label and donut parameters
- Added `edge_scale_mode` validation
- Improved roxygen documentation
- Added TNA plotting defaults and improved dotted edge style

# Sonnet 1.2.7

## Bug Fixes

- Fixed oversized nodes in `from_qgraph()` with layout override
- Fixed oval layout: use uniform scaling in `normalize_coords()`
- Fixed edge label alignment issues in `from_qgraph()`
- Fixed nrow(el) crash: qgraph Edgelist is a list, not data.frame
- Fixed misaligned edge labels by reordering per-edge vectors
- Fixed oval layout node distortion and donut fill for `from_qgraph()`

## Improvements

- Dropped qgraph cut-based edge_color/edge_width and cut param

# Sonnet 1.2.6

## New Features

- Added `donut_empty` parameter
- Added `from_qgraph()` function for qgraph compatibility

## Bug Fixes

- Fixed oval layout_info guard
- Fixed curvature extraction to only pass scalar values

## Improvements

- Scaled down edge width, label size, and arrow size from qgraph values
- Improved curvature, threshold/minimum, donut ratio, and node size defaults
- Rewrote `from_qgraph()` to read resolved graphAttributes

# Sonnet 1.2.0

## New Features

- Added `sonplot()` qgraph-compatible plotting function
- Added `layout_oval()` function for oval/ellipse layouts
- Added `layout_scale` parameter to expand/contract network layout
- Added `layout_scale = "auto"` for node-count based scaling
- Added Gephi Fruchterman-Reingold layout algorithm
- Added `edge_start_style` parameter for direction indication
- Added start segment styling to edge drawing functions

## Bug Fixes

- Fixed soplot curving and edge defaults to match splot behavior
- Fixed rescale_layout to use uniform scaling, preserving oval aspect ratio
- Fixed edge scaling producing abnormally thick edges
- Fixed invalid par(pin) error on exit

## Improvements

- Added unified parameter scaling for splot/soplot alignment
- Synced all splot() parameters to sonplot() for full feature parity

# Sonnet 1.1.0

## New Features

- Added `splot()` function for base R graphics network plotting
- Added polygon donuts, AI shapes, and SVG support
- Added shadow labels and text control options
- Added double donut nodes with separate border controls
- Added edge CI underlays and template-based edge labels
- Added comprehensive legend support for groups, edge colors, and node sizes
- Added high resolution output support
- Added edge curves mode and label styling options
- Added bidirectional arrows, loop rotation, and curve controls

## Bug Fixes

- Fixed donut rendering bug and simplified donut_color API
- Fixed arrow positioning and curve direction for qgraph-style edges
- Fixed edge label positioning to avoid overlap with edge lines
- Fixed inward curve direction for splot edges
- Fixed self-loop rendering with qgraph-style circular arcs
- Fixed arrow placement on non-square viewports
- Fixed reciprocal edge auto-separation

## Improvements

- Unified splot() and soplot() argument syntax with snake_case
- Standardized curve rules for splot and soplot
- Added layout rescaling to soplot for consistent rendering with splot
- Donut/pie content now drawn inside node boundary for visible arrows

# Sonnet 1.0.0

- Initial release of Sonnet network visualization package
