# Draw SVG Shape (Base R)

Render an SVG as a node shape using base R graphics. Falls back to
circle if rasterization fails.

## Usage

``` r
draw_svg_shape_base(x, y, size, svg_data, fill, border_color, border_width)
```

## Arguments

- x, y:

  Node center coordinates.

- size:

  Node size.

- svg_data:

  SVG data list from registry.

- fill:

  Fill color.

- border_color:

  Border color.

- border_width:

  Border width.
