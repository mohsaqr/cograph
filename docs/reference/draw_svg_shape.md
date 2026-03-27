# Draw SVG Shape (Grid)

Render an SVG as a node shape using grid graphics.

## Usage

``` r
draw_svg_shape(
  x,
  y,
  size,
  svg_data,
  fill,
  border_color,
  border_width,
  alpha = 1,
  preserve_aspect = TRUE
)
```

## Arguments

- x, y:

  Node center coordinates (NPC units).

- size:

  Node size (NPC units).

- svg_data:

  SVG data list from registry.

- fill:

  Fill color (replaces SVG fill colors).

- border_color:

  Border color.

- border_width:

  Border width.

- alpha:

  Transparency.

- preserve_aspect:

  Maintain SVG aspect ratio.

## Value

Grid grob or nullGrob if SVG unavailable.
