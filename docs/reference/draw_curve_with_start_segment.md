# Draw Curve with Optional Start Segment

Draws a curve (as lines) with an optional differently-styled start
segment. Used internally to support dashed/dotted start segments for
edge direction clarity.

## Usage

``` r
draw_curve_with_start_segment(
  x,
  y,
  col,
  lwd,
  lty,
  start_lty = 1,
  start_fraction = 0
)
```

## Arguments

- x, y:

  Vectors of curve coordinates.

- col:

  Line color.

- lwd:

  Line width.

- lty:

  Main line type.

- start_lty:

  Line type for start segment.

- start_fraction:

  Fraction of curve for start segment (0-0.5).
