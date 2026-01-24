# splot() Development Documentation

## Overview

`splot()` is a base R graphics network visualization function, designed as an alternative to `soplot()` which uses grid graphics. The goal is to replicate qgraph-style visualizations with better performance and familiar parameter names.

## Philosophy

### Why Base R Graphics?

1. **Performance**: Base R graphics (`polygon()`, `lines()`, `xspline()`, `symbols()`) are faster than grid grobs for large networks
2. **Familiarity**: Parameters mirror qgraph conventions (e.g., `vsize`, `edge.color`, `curve`)
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
| `inst/examples/splot_tests.Rmd` | Test networks (7-15 nodes) |

### Features Implemented

- All node shapes (circle, square, triangle, diamond, pentagon, hexagon, star, heart, ellipse, cross)
- Pie chart nodes
- Donut chart nodes
- Curved edges with xspline()
- Self-loops (circular arc style)
- Edge labels
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

### 1. HIGH PRIORITY: Inward Curve Direction Fix

**Problem**: When `curves="force"` is used, single (non-reciprocal) edges should curve inward toward the network center. Currently, the direction is inconsistent.

**Current Logic** (in `render_edges_base()`):
- Calculate network center as mean of layout coordinates
- Use cross product to determine which side of edge the center is on
- Adjust curve sign accordingly

**Issue**: The logic works for some edges but not all. The perpendicular vector calculation and/or cross product sign may need adjustment depending on edge orientation.

**Suggested Fix**: Look at qgraph's `PerpMid()` function and how they handle aspect ratio correction. May need to:
1. Account for plot aspect ratio in perpendicular calculation
2. Use `atan2` with aspect-corrected coordinates
3. Test with various layout types (circle, spring) to ensure consistency

### 2. HIGH PRIORITY: Resolution/DPI

**Problem**: Output resolution needs to be higher for publication quality.

**Solution**: Add `res` parameter to control DPI for raster outputs (PNG, JPEG, TIFF). Current default is 300, may need 600 for print.

### 3. MEDIUM PRIORITY: Edge Label Positioning

Edge labels on curved edges should follow the curve better. Currently they're positioned at the curve midpoint but may overlap with the edge line.

### 4. MEDIUM PRIORITY: Legend Support

Add legend for:
- Node colors (groups)
- Edge colors (positive/negative weights)
- Node sizes

### 5. LOW PRIORITY: Performance Optimization

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
splot(mat, layout="circle", curve=0.3)
```

## Related Files

- `R/soplot.R` - Grid graphics version (for comparison)
- `R/render-edges.R` - Grid edge rendering (has aspect ratio fixes that may be useful)
- `R/utils-geometry.R` - Shared geometry utilities
