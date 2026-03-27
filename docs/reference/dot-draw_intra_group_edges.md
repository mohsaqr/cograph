# Draw Intra-Group Edges with Curvature

Draws curved edges between nodes within the same group. Uses quadratic
bezier arcs with height proportional to distance, producing consistent
rounded arcs for both adjacent and distant node pairs.

## Usage

``` r
.draw_intra_group_edges(
  layout_mat,
  weights,
  group_indices,
  edge_colors,
  intra_curvature,
  orientation,
  layout_type = "bipartite",
  threshold,
  directed
)
```

## Arguments

- layout_mat:

  Layout matrix (n x 2) with x, y coordinates.

- weights:

  Full weight matrix including intra-group edges.

- group_indices:

  List of index vectors per group.

- edge_colors:

  Vector of edge colors per group.

- intra_curvature:

  Curvature amount for intra-group edges.

- orientation:

  Layout orientation ("vertical", "horizontal", "facing").

- threshold:

  Minimum weight threshold for drawing edges.

- directed:

  Logical: draw arrows?
