# Register a Custom Shape

Register a new shape that can be used for node rendering.

## Usage

``` r
register_shape(name, draw_fn)
```

## Arguments

- name:

  Character. Name of the shape.

- draw_fn:

  Function. A function that draws the shape. Should accept parameters:
  x, y, size, fill, border_color, border_width, ...

## Value

Invisible NULL.

## Examples

``` r
# Register a custom hexagon shape
register_shape("hexagon", function(x, y, size, fill, border_color, border_width, ...) {
  angles <- seq(0, 2 * pi, length.out = 7)
  grid::polygonGrob(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
  )
})
```
