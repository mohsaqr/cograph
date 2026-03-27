# cograph Scaling Constants

Central location for all scaling factors used in splot() and soplot().
These constants are calibrated to produce similar visual output to
qgraph when using equivalent parameter values.

## Usage

``` r
COGRAPH_SCALE
```

## Format

A list with the following elements:

- node_factor:

  Scale factor applied to node_size parameter

- node_default:

  Default node size when not specified

- label_default:

  Default label size (cex multiplier)

- label_coupled:

  Whether label size is coupled to node size

- edge_base:

  Base edge width

- edge_scale:

  Edge width scale factor

- edge_default:

  Default edge width

- arrow_factor:

  Scale factor for arrow sizes

- arrow_default:

  Default arrow size

## Details

The default scaling mode uses values calibrated to match qgraph visual
appearance:

- `node_size = 6` in cograph should look similar to `vsize = 6` in
  qgraph

- `label_size = 1` uses cex-style multiplier (independent of node size)

- `arrow_size = 1` produces consistent arrows between splot and soplot

Legacy mode preserves the original cograph v1.x behavior where:

- Node sizes used a 0.04 scale factor

- Label sizes were coupled to node size (vsize \* 8)

- Arrow sizes differed between splot (0.03) and soplot (0.015)
