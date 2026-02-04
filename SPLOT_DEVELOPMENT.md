# splot() Development Documentation

## Overview

`splot()` is a base R graphics network visualization function, designed as an alternative to `soplot()` which uses grid graphics. The goal is to replicate qgraph-style visualizations with better performance while maintaining API consistency with `soplot()`.

## Philosophy

### Why Base R Graphics?

1. **Performance**: Base R graphics (`polygon()`, `lines()`, `xspline()`, `symbols()`) are faster than grid grobs for large networks
2. **Consistency**: Parameters use snake_case naming matching `soplot()` for unified API
3. **Simplicity**: Direct coordinate system without NPC unit conversions
4. **xspline()**: Produces smoother, more natural curves than grid's bezier curves

### Design Decisions

1. **Curve Direction for Reciprocal Edges**: When two nodes have edges in both directions (A→B and B→A), they curve in opposite directions forming an "ellipse" shape - one curves inward, one curves outward. This prevents overlap and matches qgraph behavior.

2. **Default Behavior** (`curves=TRUE`): Only reciprocal edges are curved; single edges remain straight. This is the most common use case for network visualization.

3. **Curve Modes**:
   - `curves=FALSE`: All edges straight
   - `curves=TRUE` or `"mutual"`: Only reciprocal edges curved (default)
   - `curves="force"`: All edges curved

4. **Inward Curve Logic**: For single edges with `curves="force"`, the curve should bend toward the network center. This is determined by:
   - Calculate network center (mean of all node positions)
   - For each edge, use cross product to determine which side of the edge the center is on
   - Adjust curve sign to bend toward center

## What Has Been Done

### Files Created

| File | Purpose |
|------|---------|
| `R/splot.R` | Main function with full parameter set |
| `R/splot-edges.R` | Edge rendering (straight, curved, self-loops) |
| `R/splot-nodes.R` | Node rendering with pie/donut support |
| `R/splot-arrows.R` | Arrow head drawing |
| `R/splot-geometry.R` | Coordinate transforms, `cent_to_edge()` |
| `R/splot-params.R` | Parameter vectorization helpers |
| `R/splot-polygons.R` | Shape vertex definitions |
| `R/splot-labels.R` | Edge label template formatting helpers |
| `inst/examples/splot_tests.Rmd` | Test networks (7-15 nodes) |

### Features Implemented

- All node shapes (circle, square, triangle, diamond, pentagon, hexagon, star, heart, ellipse, cross)
- Pie chart nodes
- Donut chart nodes
- Curved edges with xspline()
- Self-loops (circular arc style)
- Edge labels (basic and template-based)
- Edge CI underlays (uncertainty visualization)
- Enhanced edge labels with templates ({est}, {range}, {p}, {stars})
- Node labels
- Weighted edge widths and colors (positive/negative)
- Arrow heads
- Bidirectional arrows
- Multiple layout algorithms (circle, spring, groups)
- File output (PNG, PDF, SVG, JPEG, TIFF)

### Curve Logic

```r
# In splot.R - determine curve signs for reciprocal edges
if (is_reciprocal[i]) {
  # Opposite directions: lower index gets positive curve
  curves_vec[i] <- if (edges$from[i] < edges$to[i]) 0.2 else -0.2
}

# In splot-edges.R - render_edges_base()
# Positive curve = bend toward center (inward)
# Negative curve = bend away from center (outward)
if (curve_i > 0) {
  # Calculate cross product to determine which side center is on
  cross <- dx * to_center_y - dy * to_center_x
  if (cross > 0) {
    curve_i <- abs(curve_i)   # Bend left toward center
  } else {
    curve_i <- -abs(curve_i)  # Bend right toward center
  }
}
```

## What Needs To Be Done

### 1. ~~HIGH PRIORITY: Inward Curve Direction Fix~~ DONE

**Problem**: When `curves="force"` is used, single (non-reciprocal) edges should curve inward toward the network center. Previously, the direction was inconsistent.

**Solution**: The issue was that `render_edges_splot()` in `splot.R` was not applying the inward curve logic that existed in `render_edges_base()`. Fixed by adding the same logic to `render_edges_splot()`:
1. Calculate network center as mean of all node positions
2. For each edge with positive curve value, use cross product to determine which side of the edge the center is on
3. Adjust curve sign: positive (bend left) if center is to the left, negative (bend right) if center is to the right
4. Negative curves (reciprocal edges curving outward) keep their original sign

### 2. ~~HIGH PRIORITY: Resolution/DPI~~ DONE

**Problem**: Output resolution needs to be higher for publication quality.

**Solution**: Added `res` parameter to control DPI for raster outputs (PNG, JPEG, TIFF). Default is now 600 DPI for publication-quality output. The parameter is passed to `grDevices::png()`, `grDevices::jpeg()`, and `grDevices::tiff()` functions.

### 3. ~~MEDIUM PRIORITY: Edge Label Positioning~~ DONE

**Problem**: Edge labels on curved edges were positioned directly on the curve, causing overlap with the edge line.

**Solution**: Added perpendicular offset to `get_edge_label_position()` in `splot-edges.R`. Labels are now offset away from the edge line:
- For curved edges: offset in the direction of the curve bulge (convex side)
- For straight edges: offset perpendicular to the edge
- Default offset of 0.03 user coordinates provides good separation

### 4. ~~MEDIUM PRIORITY: Legend Support~~ DONE

**Problem**: Legend only supported node groups, missing edge colors and node sizes.

**Solution**: Enhanced `render_legend_splot()` to support three legend components:
1. **Node groups**: Filled squares showing group colors with labels
2. **Edge colors**: Lines showing positive/negative weight colors (controlled by `legend.edge.colors` parameter, default TRUE)
3. **Node sizes**: Circles showing small/medium/large scale (controlled by `legend.node.sizes` parameter, default FALSE)

New parameters added to `splot()`:
- `legend.edge.colors`: Show positive/negative edge color legend (default TRUE)
- `legend.node.sizes`: Show node size scale legend (default FALSE)

### 5. ~~HIGH PRIORITY: Unified API with soplot()~~ DONE

**Problem**: `splot()` used qgraph-style parameter names (e.g., `vsize`, `edge.color`, `posCol`) while `soplot()` used snake_case (e.g., `node_size`, `edge_color`, `positive_color`). This inconsistency made it harder for users to switch between the two functions.

**Solution**: Updated `splot()` to use the same snake_case parameter names as `soplot()`. The full mapping:

| Old (qgraph-style) | New (snake_case) |
|-------------------|------------------|
| `vsize` | `node_size` |
| `vsize2` | `node_size2` |
| `shape` | `node_shape` |
| `color` | `node_fill` |
| `border.color` | `node_border_color` |
| `border.width` | `node_border_width` |
| `alpha` | `node_alpha` |
| `label.cex` | `label_size` |
| `label.color` | `label_color` |
| `label.position` | `label_position` |
| `pie` | `pie_values` |
| `pieColor` | `pie_colors` |
| `donut` | `donut_values` |
| `donutColor` | `donut_colors` |
| `donut.inner` | `donut_inner_ratio` |
| `donut.bg` | `donut_bg_color` |
| `donut.show.value` | `donut_show_value` |
| `donut.value.cex` | `donut_value_size` |
| `donut.value.color` | `donut_value_color` |
| `edge.color` | `edge_color` |
| `edge.width` | `edge_width` |
| `edge.alpha` | `edge_alpha` |
| `edge.labels` | `edge_labels` |
| `edge.label.cex` | `edge_label_size` |
| `edge.label.color` | `edge_label_color` |
| `edge.label.bg` | `edge_label_bg` |
| `edge.label.position` | `edge_label_position` |
| `edge.label.font` | `edge_label_fontface` |
| `lty` | `edge_style` |
| `curve` | `curvature` |
| `curveScale` | `curve_scale` |
| `curveShape` | `curve_shape` |
| `curvePivot` | `curve_pivot` |
| `asize` | `arrow_size` |
| `arrows` | `show_arrows` |
| `loopRotation` | `loop_rotation` |
| `minimum` | `threshold` |
| `posCol` | `positive_color` |
| `negCol` | `negative_color` |
| `title.cex` | `title_size` |
| `mar` | `margins` |
| `legend.position` | `legend_position` |
| `legend.cex` | `legend_size` |
| `legend.edge.colors` | `legend_edge_colors` |
| `legend.node.sizes` | `legend_node_sizes` |
| `nodeNames` | `node_names` |

Now both functions use identical parameter names for the same features.

### 6. ~~HIGH PRIORITY: Edge CI Underlays~~ DONE

**Problem**: Users need to visualize uncertainty in edge weights (e.g., confidence intervals from statistical models).

**Solution**: Added CI underlay feature that draws each edge twice:
1. **Underlay (first pass)**: thicker, dashed, semi-transparent "halo" representing uncertainty
2. **Main edge (second pass)**: normal rendering on top

New parameters for `splot()`:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `edge_ci` | numeric | NULL | CI width values (0-1 scale, bigger = more uncertainty) |
| `edge_ci_scale` | numeric | 2.0 | Width multiplier for underlay thickness |
| `edge_ci_alpha` | numeric | 0.15 | Transparency for underlay (0-1) |
| `edge_ci_color` | character | NA | Underlay color (NA = use main edge color) |
| `edge_ci_style` | numeric | 2 | Line type: 1=solid, 2=dashed, 3=dotted |
| `edge_ci_arrows` | logical | FALSE | Show arrows on underlay? |

For `soplot()`, these are available via `sn_edges()` as: `ci`, `ci_scale`, `ci_alpha`, `ci_color`, `ci_style`, `ci_arrows`.

Example:
```r
mat <- matrix(c(0, 0.5, 0.3, 0.8, 0, 0.6, 0.4, 0.7, 0), 3, 3, byrow=TRUE)
ci_vals <- c(0.2, 0.5, 0.1, 0.3, 0.4, 0.2)

splot(mat,
  edge_ci = ci_vals,
  edge_ci_scale = 2,
  edge_ci_alpha = 0.15,
  edge_ci_color = "red3",
  edge_ci_style = 2
)
```

### 7. ~~HIGH PRIORITY: Enhanced Edge Labels~~ DONE

**Problem**: Edge labels needed to display statistical information like estimates, confidence intervals, p-values, and significance stars in customizable formats.

**Solution**: Added a template-based label system with preset styles and custom templates.

**Style Presets:**
| Style | Template | Example Output |
|-------|----------|----------------|
| `"none"` | (no labels) | |
| `"estimate"` | `"{est}"` | `0.45` |
| `"full"` | `"{est} {range}"` | `0.45 [0.32, 0.58]` |
| `"range"` | `"{range}"` | `[0.32, 0.58]` |
| `"stars"` | `"{stars}"` | `**` |

**Template Placeholders:**
| Placeholder | Source | Example |
|-------------|--------|---------|
| `{est}` | edge weight | `0.45` |
| `{range}` | ci_lower + ci_upper | `[0.32, 0.58]` |
| `{low}` | ci_lower | `0.32` |
| `{up}` | ci_upper | `0.58` |
| `{p}` | p-value | `p=0.012` |
| `{stars}` | significance stars | `**` |

New parameters for `splot()`:
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `edge_label_style` | character | "none" | Preset: "none", "estimate", "full", "range", "stars" |
| `edge_label_template` | character | NULL | Custom template (overrides style) |
| `edge_label_digits` | numeric | 2 | Decimal places for estimates |
| `edge_label_oneline` | logical | TRUE | Single line format |
| `edge_label_ci_format` | character | "bracket" | CI format: "bracket" or "dash" |
| `edge_ci_lower` | numeric | NULL | Lower CI bounds |
| `edge_ci_upper` | numeric | NULL | Upper CI bounds |
| `edge_label_p` | numeric | NULL | P-values |
| `edge_label_p_digits` | numeric | 3 | Decimal places for p-values |
| `edge_label_p_prefix` | character | "p=" | Prefix for p-values |
| `edge_label_stars` | * | NULL | Stars: character, logical, or numeric p-values |

Stars conversion from p-values:
- p < 0.001 → `***`
- p < 0.01 → `**`
- p < 0.05 → `*`
- otherwise → (empty)

For `soplot()`, these are available via `sn_edges()` as: `label_style`, `label_template`, `label_digits`, `label_ci_format`, `ci_lower`, `ci_upper`, `label_p`, `label_p_digits`, `label_p_prefix`, `label_stars`.

Example:
```r
mat <- matrix(c(0, 0.5, 0.3, 0.8, 0, 0.6, 0.4, 0.7, 0), 3, 3, byrow=TRUE)

# Template with estimate and stars
splot(mat,
  edge_label_template = "{est}{stars}",
  edge_label_p = c(0.001, 0.02, 0.5, 0.008, 0.04, 0.001),
  edge_label_digits = 2
)

# Full labels with CI
splot(mat,
  edge_label_template = "{est} {range} {p}{stars}",
  edge_ci_lower = c(0.3, 0.1, 0.5, 0.2, 0.4, 0.1),
  edge_ci_upper = c(0.7, 0.5, 0.9, 0.6, 0.8, 0.5),
  edge_label_p = c(0.001, 0.02, 0.5, 0.008, 0.04, 0.001)
)
```

### 8. LOW PRIORITY: Performance Optimization

For very large networks (>500 nodes), consider:
- Batch drawing of similar elements
- Reducing xspline resolution for distant edges
- Level-of-detail rendering

## Code References

- Main curve rendering: `R/splot-edges.R:draw_curved_edge_base()` (line ~86)
- Inward direction logic: `R/splot-edges.R:render_edges_base()` (line ~380)
- Reciprocal detection: `R/splot.R` (line ~340)
- Perpendicular calculation: `R/splot-edges.R` (line ~105)

## Testing

Run the test RMarkdown:
```r
rmarkdown::render("inst/examples/splot_tests.Rmd")
```

Quick test:
```r
mat <- matrix(c(0, 0.8, 0.5, 0), 2, 2, byrow=TRUE)
rownames(mat) <- colnames(mat) <- c("A", "B")
splot(mat, layout="circle", curvature=0.3)
```

Verify unified API (both should produce identical plots):
```r
mat <- matrix(c(0, 0.8, 0.5, 0), 2, 2, byrow=TRUE)
rownames(mat) <- colnames(mat) <- c("A", "B")

# splot (base R graphics)
splot(mat, node_size=0.1, edge_color="blue", curvature=0.3)

# soplot (grid graphics)
soplot(cograph(mat), node_size=0.1, edge_color="blue", curvature=0.3)
```

## Related Files

- `R/soplot.R` - Grid graphics version (for comparison)
- `R/render-edges.R` - Grid edge rendering (has aspect ratio fixes that may be useful)
- `R/utils-geometry.R` - Shared geometry utilities
