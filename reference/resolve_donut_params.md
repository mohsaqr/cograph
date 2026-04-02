# Resolve Donut Parameters

Normalizes donut/pie overlay parameters into the canonical format
expected by render_nodes_splot(). Handles donut_fill auto-enable, list
conversion, empty-value NA replacement, color encoding (1/2/n-color),
shape inheritance, and border vectorization.

## Usage

``` r
resolve_donut_params(
  donut_fill,
  donut_values,
  donut_color,
  donut_colors,
  donut_bg_color,
  donut_shape,
  donut_border_color,
  donut_outer_border_color,
  donut_line_type,
  donut_empty,
  shapes,
  n_nodes
)
```

## Arguments

- donut_fill:

  User-specified donut fill values.

- donut_values:

  Raw donut values (deprecated path).

- donut_color:

  Donut color specification (1, 2, or n colors).

- donut_colors:

  Deprecated donut colors (list).

- donut_bg_color:

  Background color.

- donut_shape:

  Donut base shape override.

- donut_border_color:

  Border color.

- donut_outer_border_color:

  Outer border color.

- donut_line_type:

  Line type.

- donut_empty:

  Logical: render empty rings?

- shapes:

  Resolved node shapes vector.

- n_nodes:

  Number of nodes.

## Value

Named list of effective\_\* parameters.
