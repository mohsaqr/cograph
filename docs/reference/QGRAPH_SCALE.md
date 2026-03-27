# qgraph Scaling Constants (Exact Values)

Scaling constants that exactly replicate qgraph's visual formulas. Used
by splot() for qgraph-compatible network visualization.

## Usage

``` r
QGRAPH_SCALE
```

## Format

A list with the following elements:

- vsize_base:

  Base multiplier in vsize formula: 8

- vsize_decay:

  Decay constant in vsize formula: 80

- vsize_min:

  Minimum added to vsize: 1

- vsize_factor:

  Scale factor to convert vsize to user coordinates: 0.015

- esize_base:

  Base multiplier in esize formula: 15

- esize_decay:

  Decay constant in esize formula: 90

- esize_min:

  Minimum added to esize: 1

- esize_unweighted:

  Default edge width for unweighted networks: 2

- cent2edge_divisor:

  Divisor in cent2edge formula: 17.5

- cent2edge_reference:

  Reference value in cent2edge: 2.16

- cent2edge_plot_ref:

  Plot reference size: 7

- curve_ref_diagonal:

  Diagonal reference for curve normalization: sqrt(98)

- arrow_factor:

  Arrow size scale factor: 0.04
