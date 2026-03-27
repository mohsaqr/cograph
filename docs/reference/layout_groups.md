# Group-based Layout

Arrange nodes based on group membership. Groups are positioned in a
circular arrangement around the center, with nodes within each group
also arranged in a circle.

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

  A CographNetwork object.

- groups:

  Vector specifying group membership for each node. Can be numeric,
  character, or factor.

- group_positions:

  Optional list or data frame with x, y coordinates for each group
  center.

- inner_radius:

  Radius of nodes within each group (default: 0.15).

- outer_radius:

  Radius for positioning group centers (default: 0.35).

## Value

Data frame with x, y coordinates.

## Examples

``` r
# Create a network with groups
adj <- matrix(0, 9, 9)
adj[1, 2:3] <- 1; adj[2:3, 1] <- 1  # Group 1
adj[4, 5:6] <- 1; adj[5:6, 4] <- 1  # Group 2
adj[7, 8:9] <- 1; adj[8:9, 7] <- 1  # Group 3
net <- CographNetwork$new(adj)
groups <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
coords <- layout_groups(net, groups)
```
