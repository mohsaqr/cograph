# Group-based Layout

Places the nodes by group membership. The group centers lie on a circle
around (0.5, 0.5), starting at the top and proceeding counterclockwise
in the order of the group levels. A single group is centered at (0.5,
0.5). The nodes of each group lie on a circle around their group center,
and a group with one node is placed at its center.

## Usage

``` r
layout_groups(
  network,
  groups,
  group_positions = NULL,
  inner_radius = 0.15,
  outer_radius = 0.35
)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- groups:

  Vector of group memberships, one per node (numeric, character, or
  factor). A length different from the number of nodes raises an error.

- group_positions:

  Optional list or data frame with columns `x` and `y` giving the group
  centers, one row per group level.

- inner_radius:

  Radius of the circle of nodes within each group. Default 0.15.

- outer_radius:

  Radius of the circle of group centers. Default 0.35.

## Value

A data frame with columns `x` and `y` and one row per node, in node
order.

## Examples

``` r
layout_groups(CographNetwork$new(regulation_net),
  groups = rep(c("A", "B"), each = 5))
#>            x          y
#> 1  0.5000000 1.00000000
#> 2  0.3573415 0.89635255
#> 3  0.4118322 0.72864745
#> 4  0.5881678 0.72864745
#> 5  0.6426585 0.89635255
#> 6  0.5000000 0.30000000
#> 7  0.3573415 0.19635255
#> 8  0.4118322 0.02864745
#> 9  0.5881678 0.02864745
#> 10 0.6426585 0.19635255
```
