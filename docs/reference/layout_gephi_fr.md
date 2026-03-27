# Gephi Fruchterman-Reingold Layout

Force-directed layout that replicates Gephi's Fruchterman-Reingold
algorithm. This is a strict port of the Java implementation from Gephi's
source code, with additional improvements for reproducibility and
flexibility.

## Usage

``` r
layout_gephi_fr(
  g,
  area = 10000,
  gravity = 1,
  speed = 1,
  niter = 100,
  seed = NULL,
  initial = NULL,
  normalize = TRUE,
  gravity_mode = c("linear", "degree", "none"),
  cooling_mode = c("constant", "vcf", "linear"),
  anchor_strength = 0
)
```

## Arguments

- g:

  An igraph graph object.

- area:

  Area parameter controlling node spread. Default 10000.

- gravity:

  Gravity force pulling nodes toward center. Default 1.0. (Note: Reduced
  from Gephi's default of 10.0 to prevent circular layouts)

- speed:

  Speed/cooling parameter. Default 1.0.

- niter:

  Number of iterations. Default 100.

- seed:

  Random seed for reproducibility. Default NULL.

- initial:

  Optional initial coordinates (matrix or data frame). Useful for
  warm-starting or animations.

- normalize:

  Logical. If TRUE (default), normalize output to \[0,1\] range. If
  FALSE, return raw Gephi-scale coordinates.

- gravity_mode:

  Gravity behavior: "linear" (default, standard gravity), "degree"
  (high-degree nodes feel stronger gravity, creates hub structure), or
  "none" (no gravity, like igraph FR).

- cooling_mode:

  Cooling schedule: "constant" (default, no cooling), "vcf" (Variable
  Cooling Factor - adapts based on movement), or "linear" (linear
  decrease over iterations).

- anchor_strength:

  Strength of force pulling nodes toward initial positions (default: 0).
  Only applies when `initial` is provided.

## Value

A matrix with x,y coordinates for each node.

## Details

This layout is a direct port of Gephi's ForceAtlas algorithm variant of
Fruchterman-Reingold. Key differences from igraph's layout_with_fr:

- Uses Gephi's specific constants (SPEED_DIVISOR=800,
  AREA_MULTIPLICATOR=10000)

- Includes configurable gravity toward center

- Different cooling/speed mechanism

- Supports multiple gravity modes for different layout styles
