# Compute Connectivity-Based Jitter (Vertical Layout)

For vertical layouts (top/bottom rows). Nodes with more cross-group
connections are jittered vertically toward center.

## Usage

``` r
compute_connectivity_jitter_vertical(
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

  Indices of group 1 nodes (top).

- g2_idx:

  Indices of group 2 nodes (bottom).

- amount:

  Maximum jitter amount. Default 0.8.

- side:

  Which group(s) to jitter: "group1", "group2", or "both".

## Value

Numeric vector of y-offsets for each node.
