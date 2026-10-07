# Fruchterman-Reingold Spring Layout

Computes node positions with the Fruchterman-Reingold force-directed
algorithm. Nodes connected by edges attract each other and all nodes
repel each other. Attraction along an edge is scaled by the absolute
edge weight. Duplicate edges between the same pair of nodes, in either
direction, are merged by summing their weights.

## Usage

``` r
layout_spring(
  network,
  iterations = 200,
  cooling = 0.95,
  repulsion = 1.5,
  attraction = 1,
  seed = NULL,
  initial = NULL,
  max_displacement = NULL,
  anchor_strength = 0,
  area = 1.5,
  gravity = 0,
  init = c("random", "circular"),
  cooling_mode = c("exponential", "vcf", "linear"),
  ...
)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- iterations:

  Number of iterations (default: 200).

- cooling:

  Factor by which the temperature is multiplied after each iteration
  when `cooling_mode = "exponential"` (default: 0.95).

- repulsion:

  Repulsion constant (default: 1.5).

- attraction:

  Attraction constant (default: 1).

- seed:

  Random seed for the random initial positions. NULL (default) uses the
  current random state. When a seed is given, the caller's random state
  is restored on exit.

- initial:

  Optional initial coordinates, a matrix or a data frame with columns
  `x` and `y`. When supplied, `init` is ignored. For animations, pass
  the previous frame's layout to obtain smooth transitions.

- max_displacement:

  Maximum distance a node can move from its initial position (default:
  NULL, no limit). Applies only when `initial` is supplied. In that case
  the coordinates are returned without the final rescaling. Values such
  as 0.05 to 0.1 suit animations.

- anchor_strength:

  Strength of the force pulling nodes toward their initial positions
  (default: 0). Higher values (for example 0.5 to 2) keep nodes closer
  to their starting positions. Applies only when `initial` is supplied.

- area:

  Area parameter (default: 1.5). It sets the ideal edge length \\k =
  \sqrt{area / n}\\ and the initial temperature \\\sqrt{area} / 10\\.
  Because the final coordinates are rescaled, `area` changes the
  relative arrangement of the nodes and leaves the overall extent
  unchanged.

- gravity:

  Strength of the force pulling nodes toward the centroid of the layout
  (default: 0). Higher values (for example 0.5 to 2) give a more compact
  layout.

- init:

  Initialization method, `"random"` (default) or `"circular"`.

- cooling_mode:

  Cooling schedule. `"exponential"` (default) multiplies the temperature
  by `cooling` after each iteration. `"vcf"` (variable cooling factor)
  multiplies it by 0.9 when the mean displacement is below \\0.1k\\ and
  by 0.99 otherwise. `"linear"` multiplies it by \\1 - t / T\\ at
  iteration \\t\\ of \\T\\.

- ...:

  Additional arguments (ignored).

## Value

A data frame with columns `x` and `y` and one row per node. The
coordinates are centered on their mean and scaled by a common factor
into \\\[0.05, 0.95\]\\, which preserves the aspect ratio of the layout.
A network without edges returns the initial positions, and a single node
is placed at (0.5, 0.5).

## Examples

``` r
layout_spring(CographNetwork$new(regulation_net), seed = 42)
#>            x         y
#> 1  0.7964244 0.5992607
#> 2  0.3632561 0.5210930
#> 3  0.2023914 0.4061824
#> 4  0.7441472 0.3580494
#> 5  0.4212874 0.8198960
#> 6  0.5880101 0.6367377
#> 7  0.6274058 0.9500000
#> 8  0.4826823 0.1498104
#> 9  0.2638862 0.1976380
#> 10 0.5105091 0.3613325
```
