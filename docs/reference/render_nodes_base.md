# Render All Nodes

Renders all nodes in the network.

## Usage

``` r
render_nodes_base(
  layout,
  vsize,
  vsize2 = NULL,
  shape = "circle",
  color = "#4A90D9",
  border.color = "#2C5AA0",
  border.width = 1,
  pie = NULL,
  pieColor = NULL,
  donut = NULL,
  donutColor = NULL,
  labels = NULL,
  label.cex = 1,
  label.color = "black"
)
```

## Arguments

- layout:

  Matrix with x, y columns.

- vsize:

  Vector of node sizes.

- vsize2:

  Vector of secondary sizes (for ellipse).

- shape:

  Vector of shape names.

- color:

  Vector of fill colors.

- border.color:

  Vector of border colors.

- border.width:

  Vector of border widths.

- pie:

  List of pie value vectors (one per node) or NULL.

- pieColor:

  List of pie color vectors or NULL.

- donut:

  List of donut values or NULL.

- donutColor:

  List of donut color vectors or NULL.

- labels:

  Vector of labels or NULL.

- label.cex:

  Vector of label sizes.

- label.color:

  Vector of label colors.
