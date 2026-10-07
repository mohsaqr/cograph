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

  A CographNetwork or cograph_network object.

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
layout_oval(CographNetwork$new(regulation_net), ratio = 1.5)
#>             x         y
#> 1  0.78795479 0.7642238
#> 2  0.96592064 0.6009245
#> 3  0.96592064 0.3990755
#> 4  0.78795479 0.2357762
#> 5  0.50000000 0.1734014
#> 6  0.21204521 0.2357762
#> 7  0.03407936 0.3990755
#> 8  0.03407936 0.6009245
#> 9  0.21204521 0.7642238
#> 10 0.50000000 0.8265986
```
