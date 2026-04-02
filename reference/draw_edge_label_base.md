# Draw Edge Label

Renders a label on an edge.

## Usage

``` r
draw_edge_label_base(
  x,
  y,
  label,
  cex = 0.8,
  col = "gray30",
  bg = "white",
  font = 1,
  shadow = FALSE,
  shadow_color = "gray40",
  shadow_offset = 0.5,
  shadow_alpha = 0.5
)
```

## Arguments

- x, y:

  Label position coordinates.

- label:

  Text to display.

- cex:

  Character expansion factor.

- col:

  Text color.

- bg:

  Background color (or NA for none).

- font:

  Font face.

- shadow:

  Logical or character: FALSE for none, TRUE or "shadow" for drop
  shadow, "halo" for outline rim around text.

- shadow_color:

  Shadow/halo color.

- shadow_offset:

  Shadow/halo offset distance.

- shadow_alpha:

  Shadow/halo transparency.
