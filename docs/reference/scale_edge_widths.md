# Scale Edge Widths Based on Weights

Unified edge width scaling function that supports multiple scaling
modes, two-tier cutoff system (like qgraph), and output range
specification.

## Usage

``` r
scale_edge_widths(
  weights,
  esize = NULL,
  n_nodes = NULL,
  directed = FALSE,
  mode = "linear",
  maximum = NULL,
  minimum = 0,
  cut = NULL,
  range = c(0.5, 4)
)
```

## Arguments

- weights:

  Numeric vector of edge weights.

- esize:

  Base edge size. NULL uses adaptive sizing based on n_nodes.

- n_nodes:

  Number of nodes (for adaptive esize calculation).

- directed:

  Whether network is directed (affects adaptive esize).

- mode:

  Scaling mode: "linear", "log", "sqrt", or "rank".

- maximum:

  Max weight for normalization. NULL for auto-detect.

- minimum:

  Min weight threshold. Edges below this get minimum width.

- cut:

  Two-tier cutoff threshold. NULL = auto (75th percentile), 0 = disabled
  (continuous scaling), positive number = manual threshold.

- range:

  Output width range as c(min_width, max_width).

## Value

Numeric vector of scaled edge widths.

## Details

### Scaling Modes

- **linear** (default): Direct proportional scaling, matches qgraph
  behavior.

- **log**: Logarithmic scaling for wide weight ranges. Uses log1p for
  stability.

- **sqrt**: Square root scaling for moderate compression.

- **rank**: Rank-based scaling for equal visual spacing regardless of
  weight distribution.

### Two-Tier System (cut parameter)

When cut \> 0, edges are divided into two tiers:

- Below cut: Minimal width variation (20% of range)

- Above cut: Full width scaling (80% of range)

This matches qgraph's behavior where weak edges are visually
de-emphasized.
