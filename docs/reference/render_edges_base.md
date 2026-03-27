# Render All Edges

Renders all edges in the network.

## Usage

``` r
render_edges_base(
  edges,
  layout,
  node_sizes,
  shapes = "circle",
  edge.color = "gray50",
  edge.width = 1,
  lty = 1,
  curve = 0,
  curvePivot = 0.5,
  arrows = TRUE,
  asize = 0.02,
  bidirectional = FALSE,
  loopRotation = NULL,
  edge.labels = NULL,
  edge.label.cex = 0.8,
  edge.label.bg = "white",
  edge.label.position = 0.5
)
```

## Arguments

- edges:

  Edge data frame with from, to columns.

- layout:

  Matrix with x, y columns.

- node_sizes:

  Vector of node sizes.

- shapes:

  Vector of node shapes.

- edge.color:

  Vector of edge colors.

- edge.width:

  Vector of edge widths.

- lty:

  Vector of line types.

- curve:

  Vector of curvatures.

- curvePivot:

  Vector of curve pivot positions.

- arrows:

  Logical or vector: draw arrows?

- asize:

  Arrow size.

- bidirectional:

  Logical or vector: bidirectional arrows?

- loopRotation:

  Vector of loop rotation angles.

- edge.labels:

  Vector of edge labels or NULL.

- edge.label.cex:

  Label size.

- edge.label.bg:

  Label background color.

- edge.label.position:

  Label position along edge.
