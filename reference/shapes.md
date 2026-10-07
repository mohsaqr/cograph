# Node Shapes

Node shapes are stored in a registry and selected by name through the
`node_shape` argument of
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) or the
`shape` argument of
[`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md).
`register_shape()` adds a shape given by a drawing function, and
`get_shape()` returns the drawing function of a registered shape.
`list_shapes()` returns the names of all registered shapes. A shape
added with `register_shape()` is a grid shape and is used by
[`soplot()`](https://sonsoles.me/cograph/reference/soplot.md) only.
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) plots its
built-in shapes and SVG shapes. `register_svg_shape()` adds a shape
defined by an SVG file or an inline SVG string, `list_svg_shapes()`
returns the names of the registered SVG shapes, and
`unregister_svg_shape()` removes one. SVG shapes are used by both
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot()`](https://sonsoles.me/cograph/reference/soplot.md).
Registering an existing name replaces that shape for the rest of the
session.

## Usage

``` r
register_shape(name, draw_fn)

get_shape(name)

list_shapes()

register_svg_shape(name, svg_source)

list_svg_shapes()

unregister_svg_shape(name)
```

## Arguments

- name:

  Character. The name of the shape.

- draw_fn:

  A function that renders the shape. It receives the arguments `x`, `y`,
  `size`, `fill`, `border_color`, `border_width`, `alpha` and `...` and
  returns a grid grob.

- svg_source:

  Character. A path to an SVG file or an inline SVG string.

## Value

`register_shape()` and `register_svg_shape()` return `NULL` invisibly.
`get_shape()` returns the drawing function, or `NULL` if no shape has
that name. `list_shapes()` and `list_svg_shapes()` return a character
vector of shape names. `unregister_svg_shape()` returns `TRUE` invisibly
if the shape was removed and `FALSE` if it was not found.

## Examples

``` r
list_shapes()
#>  [1] "circle"           "square"           "triangle"         "diamond"         
#>  [5] "pentagon"         "hexagon"          "ellipse"          "heart"           
#>  [9] "star"             "pie"              "donut"            "polygon_donut"   
#> [13] "donut_pie"        "double_donut_pie" "cross"            "plus"            
#> [17] "neural"           "chip"             "robot"            "brain"           
#> [21] "network"          "database"         "cloud"            "gear"            
#> [25] "rectangle"        "none"            
splot(regulation_net, node_shape = "diamond")
```
