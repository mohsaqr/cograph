# Circular Layout

Places the nodes at evenly spaced positions on a circle of radius 0.4
centered at (0.5, 0.5). A single node is placed at the center.

## Usage

``` r
layout_circle(network, order = NULL, start_angle = pi/2, clockwise = TRUE, ...)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- order:

  Optional vector of node indices or labels. Its i-th element is the
  node placed at the i-th position. Labels are matched only for a
  `CographNetwork` object. An order of the wrong length, or with
  unmatched labels, raises a warning and the default order is used.

- start_angle:

  Angle in radians of the reference position. Default `pi/2` (top of the
  circle).

- clockwise:

  Logical. Default `TRUE` places the positions clockwise, with the last
  position at `start_angle` and the first position one step clockwise of
  it. `FALSE` places the first position at `start_angle` and continues
  counterclockwise.

- ...:

  Ignored.

## Value

A data frame with columns `x` and `y` and one row per node, in node
order.

## Examples

``` r
layout_circle(CographNetwork$new(regulation_net))
#>            x         y
#> 1  0.7351141 0.8236068
#> 2  0.8804226 0.6236068
#> 3  0.8804226 0.3763932
#> 4  0.7351141 0.1763932
#> 5  0.5000000 0.1000000
#> 6  0.2648859 0.1763932
#> 7  0.1195774 0.3763932
#> 8  0.1195774 0.6236068
#> 9  0.2648859 0.8236068
#> 10 0.5000000 0.9000000
```
