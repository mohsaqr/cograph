# Fruchterman-Reingold Spring Layout

Compute node positions using the Fruchterman-Reingold force-directed
algorithm. Nodes connected by edges are attracted to each other while
all nodes repel each other.

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

  A CographNetwork object.

- iterations:

  Number of iterations (default: 200).

- cooling:

  Rate of temperature decrease for exponential cooling (default: 0.95).

- repulsion:

  Repulsion constant (default: 1.5).

- attraction:

  Attraction constant (default: 1).

- seed:

  Random seed for reproducibility.

- initial:

  Optional initial coordinates (matrix or data frame). For animations,
  pass the previous frame's layout to ensure smooth transitions.

- max_displacement:

  Maximum distance a node can move from its initial position (default:
  NULL = no limit). Useful for animations to prevent large jumps between
  frames. Values like 0.05-0.1 work well.

- anchor_strength:

  Strength of force pulling nodes toward initial positions (default: 0).
  Higher values (e.g., 0.5-2) keep nodes closer to their starting
  positions. Only applies when `initial` is provided.

- area:

  Area parameter controlling node spread (default: 1.5). Higher values
  spread nodes further apart.

- gravity:

  Gravity force pulling nodes toward center (default: 0). Higher values
  (e.g., 0.5-2) prevent nodes from drifting apart.

- init:

  Initialization method: "random" (default) or "circular".

- cooling_mode:

  Cooling schedule: "exponential" (default, uses `cooling` parameter),
  "vcf" (Variable Cooling Factor - adapts based on movement), or
  "linear" (linear decrease over iterations).

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4)
net <- CographNetwork$new(adj)
coords <- layout_spring(net, seed = 42)

# For animations: use previous layout as initial with constraints
coords2 <- layout_spring(net, initial = coords, max_displacement = 0.05)

# With gravity to keep nodes centered
coords3 <- layout_spring(net, gravity = 0.5, area = 2, seed = 42)

# With circular initialization and VCF cooling
coords4 <- layout_spring(net, init = "circular", cooling_mode = "vcf", seed = 42)
```
