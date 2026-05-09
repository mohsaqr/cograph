# Scale Edge Widths Based on Weights

Unified edge width scaling function that supports multiple scaling modes
and output range specification.

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
  range = c(0.5, 4),
  visual_scale = NULL
)
```

## Arguments

- weights:

  Numeric vector of edge weights.

- esize:

  Maximum edge size. If NULL, `range[2]` is used.

- n_nodes:

  Number of nodes. Accepted for caller compatibility; not used by this
  scaler.

- directed:

  Whether network is directed. Accepted for caller compatibility; not
  used by this scaler.

- mode:

  Scaling mode: "linear", "log", "sqrt", or "rank".

- maximum:

  Max weight for normalization. NULL for auto-detect.

- minimum:

  Min weight threshold. Edges below this get minimum width.

- cut:

  Accepted for caller compatibility. Width scaling is continuous in the
  current implementation; cutoff handling is performed by callers for
  other aesthetics such as transparency.

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
