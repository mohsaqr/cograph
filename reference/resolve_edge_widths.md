# Resolve Edge Widths

Determines edge widths based on weights or explicit values. Supports
multiple scaling modes, two-tier cutoff, and output range specification.

## Usage

``` r
resolve_edge_widths(
  edges,
  edge.width = NULL,
  esize = NULL,
  n_nodes = NULL,
  directed = FALSE,
  maximum = NULL,
  minimum = 0,
  cut = NULL,
  edge_width_range = NULL,
  edge_scale_mode = NULL,
  scaling = "default",
  base_width = NULL,
  scale_factor = NULL
)
```

## Arguments

- edges:

  Edge data frame.

- edge.width:

  User-specified width(s) or NULL.

- esize:

  Base edge size. NULL uses adaptive sizing based on n_nodes.

- n_nodes:

  Number of nodes (for adaptive esize calculation).

- directed:

  Whether network is directed.

- maximum:

  Maximum weight for scaling (NULL for auto).

- minimum:

  Minimum weight threshold.

- cut:

  Two-tier cutoff. NULL = auto (75th pct), 0 = disabled.

- edge_width_range:

  Output width range c(min, max).

- edge_scale_mode:

  Scaling mode: "linear", "log", "sqrt", "rank".

- scaling:

  Scaling mode for constants: "default" or "legacy".

- base_width:

  Legacy: Base width value.

- scale_factor:

  Legacy: Width scaling factor.

## Value

Vector of widths for each edge.
