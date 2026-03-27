# Draw a Single Node

Renders a node at the specified position with given aesthetics.

## Usage

``` r
draw_node_base(
  x,
  y,
  size,
  size2 = NULL,
  shape = "circle",
  col = "#4A90D9",
  border.col = "#2C5AA0",
  border.width = 1,
  ...
)
```

## Arguments

- x, y:

  Node center coordinates.

- size:

  Node radius in user coordinates.

- size2:

  Secondary size (for ellipse height).

- shape:

  Node shape name.

- col:

  Fill color.

- border.col:

  Border color.

- border.width:

  Border line width.

- ...:

  Additional parameters.
