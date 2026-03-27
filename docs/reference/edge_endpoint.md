# Calculate Edge Endpoint on Node Border

Calculates the point where an edge should meet the node border. Uses
plain NPC units to match circleGrob borders.

## Usage

``` r
edge_endpoint(
  node_x,
  node_y,
  other_x,
  other_y,
  node_size,
  shape = "circle",
  x_scale = 1,
  y_scale = 1
)
```

## Arguments

- node_x, node_y:

  Node center in npc.

- other_x, other_y:

  Other endpoint in npc.

- node_size:

  Node radius in npc units.

- shape:

  Node shape.

- x_scale, y_scale:

  Aspect ratio correction factors.

## Value

List with x, y coordinates in npc.
