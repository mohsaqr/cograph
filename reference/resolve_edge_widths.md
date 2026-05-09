# Resolve Edge Widths

Determines edge widths based on weights or explicit values. Supports
multiple scaling modes and output range specification.

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
  scale_factor = NULL,
  visual_scale = NULL
)
```

## Arguments

- edges:

  Edge data frame.

- edge.width:

  User-specified width(s) or NULL.

- esize:

  Optional maximum edge size. NULL uses `edge_width_range`.

- n_nodes:

  Number of nodes, passed through to the scaler for compatibility.

- directed:

  Whether network is directed.

- maximum:

  Maximum weight for scaling (NULL for auto).

- minimum:

  Minimum weight threshold.

- cut:

  Accepted for compatibility. Current width scaling is continuous;
  cutoff effects are handled by callers for other aesthetics.

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
