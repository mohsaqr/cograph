# Compute Circular Layout

Positions nodes along arcs of a circle, with each group occupying one
arc. Groups are separated by gaps controlled by angle_spacing.

## Usage

``` r
compute_circular_layout(
  node_list,
  lab,
  group_indices,
  n_groups,
  angle_spacing = 0.15,
  scale = 1
)
```

## Arguments

- node_list:

  List of n character vectors.

- lab:

  Node labels from model.

- group_indices:

  List of index vectors for each group.

- n_groups:

  Number of groups.

- angle_spacing:

  Gap between groups as fraction of arc (0-1). Default 0.15.

- scale:

  Scaling factor for radius. Default 1.

## Value

List with x and y position vectors.
