# Saqr Layout (Start/End transition flow)

Places the nodes of a directed transition network in rows between a
Start and an End node (Saqr et al., LAK25). The Start node is alone on
the top row and the End node, when present, is alone on the bottom row.
The other nodes are ranked by the weight of the edge they receive from
Start, with the strongest nearest Start. They are split into two middle
rows when there are at most 10 of them and into three rows otherwise. A
sine envelope narrows the rows near Start and End, which gives the
layout a lens shape. The first middle row is offset in a zig-zag
pattern.

## Usage

``` r
layout_saqr(network, start = "Start", end = "End", jitter = 0.32, ...)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- start:

  Label of the Start node (default `"Start"`). When the label is not
  found, the node with the largest sum of outgoing weights is used.

- end:

  Label of the End node (default `"End"`). The End row is omitted when
  the label is not found.

- jitter:

  Numeric in `[0, 1]`. Zig-zag amount applied to the first middle row,
  as a fraction of the row spacing (default 0.32).

- ...:

  Additional arguments (ignored).

## Value

Data frame with `x`, `y` coordinates, one row per node.

## Examples

``` r
layout_saqr(CographNetwork$new(regulation_net), start = "Explore",
  end = "Share")
#>            x         y
#> 1  0.5000000 1.0000000
#> 2  0.3556624 0.5600000
#> 3  0.6443376 0.7733333
#> 4  0.9330127 0.5600000
#> 5  0.0669873 0.7733333
#> 6  0.0669873 0.3333333
#> 7  0.3556624 0.3333333
#> 8  0.6443376 0.3333333
#> 9  0.9330127 0.3333333
#> 10 0.5000000 0.0000000
```
