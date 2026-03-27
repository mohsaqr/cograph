# Wrapper for Gephi FR Layout (for layout registry)

Wrapper for Gephi FR Layout (for layout registry)

## Usage

``` r
compute_layout_gephi_fr(
  network,
  area = 10000,
  gravity = 1,
  speed = 1,
  niter = 100,
  seed = NULL,
  initial = NULL,
  normalize = TRUE,
  gravity_mode = "linear",
  cooling_mode = "constant",
  anchor_strength = 0,
  ...
)
```

## Arguments

- network:

  A cograph_network object.

- area:

  Area parameter. Default 10000.

- gravity:

  Gravity force. Default 1.0.

- speed:

  Speed parameter. Default 1.0.

- niter:

  Number of iterations. Default 100.

- seed:

  Random seed for reproducibility.

- initial:

  Optional initial coordinates.

- normalize:

  Normalize output to \[0,1\]. Default TRUE.

- gravity_mode:

  Gravity behavior: "linear", "degree", or "none".

- cooling_mode:

  Cooling schedule: "constant", "vcf", or "linear".

- anchor_strength:

  Anchor force strength for animations.

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.
