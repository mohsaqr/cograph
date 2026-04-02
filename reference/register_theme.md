# Register a Custom Theme

Register a new theme for network visualization.

## Usage

``` r
register_theme(name, theme)
```

## Arguments

- name:

  Character. Name of the theme.

- theme:

  A CographTheme object or a list of theme parameters.

## Value

Invisible NULL.

## Examples

``` r
# Register a custom theme
register_theme("custom", list(
  background = "white",
  node_fill = "steelblue",
  node_border = "navy",
  edge_color = "gray50"
))
```
