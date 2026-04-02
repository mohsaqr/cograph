# Resolve Label Sizes

Determines label sizes, either independent (new default) or coupled to
node size (legacy).

## Usage

``` r
resolve_label_sizes(label_size, node_size_usr, n, scaling = "default")
```

## Arguments

- label_size:

  User-specified label size(s) or NULL.

- node_size_usr:

  Node sizes in user coordinates (for legacy coupled mode).

- n:

  Number of nodes.

- scaling:

  Scaling mode: "default" or "legacy".

## Value

Vector of label sizes (cex values).
