# Compute Connectivity-Based Jitter (Horizontal Layout)

For horizontal layouts (left/right columns). Nodes with more cross-group
connections are jittered horizontally toward center.

## Usage

``` r
compute_connectivity_jitter_horizontal(
  weights,
  g1_idx,
  g2_idx,
  amount = 0.8,
  side = "group1"
)
```

## Arguments

- weights:

  Weight matrix.

- g1_idx:

  Indices of group 1 nodes.

- g2_idx:

  Indices of group 2 nodes.

- amount:

  Maximum jitter amount. Default 0.8.

- side:

  Which group(s) to jitter: "group1", "group2", or "both".

## Value

Numeric vector of x-offsets for each node.
