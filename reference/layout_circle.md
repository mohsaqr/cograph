# Circular Layout

Arrange nodes evenly spaced around a circle.

## Usage

``` r
layout_circle(network, order = NULL, start_angle = pi/2, clockwise = TRUE, ...)
```

## Arguments

- network:

  A CographNetwork object.

- order:

  Optional vector specifying node order (indices or labels).

- start_angle:

  Starting angle in radians (default: pi/2 for top).

- clockwise:

  Logical. Arrange nodes clockwise? Default TRUE.

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)
coords <- layout_circle(net)
```
