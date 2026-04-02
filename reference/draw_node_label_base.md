# Draw Node Label

Renders a text label at or near a node.

## Usage

``` r
draw_node_label_base(
  x,
  y,
  label,
  cex = 1,
  col = "black",
  font = 1,
  family = "sans",
  hjust = 0.5,
  vjust = 0.5,
  srt = 0,
  pos = NULL,
  offset = 0.5
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

- font:

  Font face (1=plain, 2=bold, 3=italic, 4=bold italic).

- family:

  Font family ("sans", "serif", "mono").

- hjust:

  Horizontal justification (0=left, 0.5=center, 1=right).

- vjust:

  Vertical justification (0=bottom, 0.5=center, 1=top).

- srt:

  String rotation angle in degrees.

- pos:

  Position relative to point (NULL=centered, 1=below, 2=left, 3=above,
  4=right).

- offset:

  Offset distance when using pos.
