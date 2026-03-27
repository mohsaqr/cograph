# Resolve Node Sizes

Converts vsize parameter to user coordinate sizes.

## Usage

``` r
resolve_node_sizes(
  vsize,
  n,
  default_size = NULL,
  scale_factor = NULL,
  scaling = "default"
)
```

## Arguments

- vsize:

  User-specified node size(s).

- n:

  Number of nodes.

- default_size:

  Default size if NULL (uses scale constants if NULL).

- scale_factor:

  Scale factor to apply (uses scale constants if NULL).

- scaling:

  Scaling mode: "default" or "legacy".

## Value

Vector of node sizes.
