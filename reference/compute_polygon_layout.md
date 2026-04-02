# Compute Polygon Layout

Positions nodes along edges of a regular n-sided polygon. Each group is
placed along one edge. Edges are offset outward from vertices to create
empty angles at corners.

## Usage

``` r
compute_polygon_layout(
  node_list,
  lab,
  group_indices,
  n_sides,
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

- n_sides:

  Number of sides (groups).

- angle_spacing:

  How far to push edges away from vertices (0-1). Default 0.15.

- scale:

  Scaling factor for radius. Default 1.

## Value

List with x and y position vectors.
