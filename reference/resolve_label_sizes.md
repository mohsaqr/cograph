# Resolve Label Sizes

Determines label sizes. Default links label cex to node size (qgraph
invariant: label.cex is a function of vsize so the node-to-label ratio
is locked by construction, and device compensation applied uniformly to
both keeps the ratio stable across canvases).

## Usage

``` r
resolve_label_sizes(
  label_size,
  node_size_usr,
  n,
  scaling = "default",
  visual_scale = NULL,
  node_size = NULL
)
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

- visual_scale:

  Optional visual-scale list (from compute_visual_scale). When non-NULL
  and the caller did not pass `label_size`, the default label cex is
  multiplied by `visual_scale$scale` so label pixel size tracks the
  output canvas. User-explicit `label_size` always wins and is returned
  verbatim — the precedence rule.

- node_size:

  Raw user-supplied node_size (pre-scale-factor). When default (7),
  yields label cex 1.0 — backward-compatible. When the user doubles
  node_size, labels double too; when they halve node_size, labels halve.
  Falls back to the scale constant's `node_default` when NULL.

## Value

Vector of label sizes (cex values).
