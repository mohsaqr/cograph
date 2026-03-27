# Oval Layout

Arrange nodes evenly spaced around an ellipse. This creates an
oval-shaped network layout that is wider than it is tall (or vice versa
depending on ratio).

## Usage

``` r
layout_oval(
  network,
  ratio = 1.5,
  order = NULL,
  start_angle = pi/2,
  clockwise = TRUE,
  rotation = 0,
  ...
)
```

## Arguments

- network:

  A CographNetwork object.

- ratio:

  Aspect ratio (width/height). Values \> 1 create horizontal ovals,
  values \< 1 create vertical ovals. Default 1.5.

- order:

  Optional vector specifying node order (indices or labels).

- start_angle:

  Starting angle in radians (default: pi/2 for top).

- clockwise:

  Logical. Arrange nodes clockwise? Default TRUE.

- rotation:

  Rotation angle in radians to tilt the entire oval. Default 0.

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)
coords <- layout_oval(net, ratio = 1.5)
```
