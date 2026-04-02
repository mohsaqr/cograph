# Register Custom SVG Shape

Register an SVG file or string as a custom node shape.

## Usage

``` r
register_svg_shape(name, svg_source)
```

## Arguments

- name:

  Character: unique name for this shape (used in node_shape parameter).

- svg_source:

  Character: path to SVG file OR inline SVG string.

## Value

Invisible NULL. The shape is registered for use with sn_nodes().

## Examples

``` r
if (FALSE) { # \dontrun{
# Register from file
register_svg_shape("custom_icon", "path/to/icon.svg")

# Register from inline SVG
register_svg_shape("simple_star",
  '<svg viewBox="0 0 100 100">
    <polygon points="50,5 20,99 95,39 5,39 80,99" fill="currentColor"/>
  </svg>')

# Use in network
cograph(adj) |> sn_nodes(shape = "custom_icon")
} # }
```
