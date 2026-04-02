# qgraph Edge Width Scaling (EXACT)

Scales edge weights to widths using qgraph's exact formula. Output range
is 1 to esize for continuous scaling (cut = 0).

## Usage

``` r
qgraph_scale_edge_widths(
  weights,
  minimum = 0,
  maximum = NULL,
  cut = 0,
  esize = NULL
)
```

## Arguments

- weights:

  Numeric vector of edge weights.

- minimum:

  Minimum weight threshold.

- maximum:

  Maximum weight for normalization.

- cut:

  Two-tier cutoff threshold. 0 = continuous scaling.

- esize:

  Maximum edge width.

## Value

Numeric vector of scaled edge widths.
