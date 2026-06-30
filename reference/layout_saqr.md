# Saqr Layout (Start/End transition flow)

Port of the Dynalytics Desktop "saqr" layout (Saqr et al., LAK25).
Designed for directed transition networks: the Start node sits alone on
the top row, the End node (if present) alone on the bottom row, and
every other node is ranked by its outgoing weight from Start (strongest
connections nearest Start) and split into 2 middle rows (\<= 10 middle
nodes) or 3 (\> 10). A sine envelope narrows the rows near Start/End for
a lens-shaped silhouette, and the first middle row is zig-zag jittered.

## Usage

``` r
layout_saqr(network, start = "Start", end = "End", jitter = 0.32, ...)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- start:

  Label of the Start node (default `"Start"`). Falls back to the highest
  out-degree node when the label is not found.

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

## Details

If the `start` label is absent the highest out-degree node is used. The
End row is only drawn when the `end` label is present.

## Examples

``` r
adj <- matrix(0, 5, 5,
  dimnames = list(c("Start", "A", "B", "C", "End"),
                  c("Start", "A", "B", "C", "End")))
adj["Start", "A"] <- 5; adj["Start", "B"] <- 3; adj["Start", "C"] <- 1
adj["A", "End"] <- 2; adj["B", "End"] <- 4; adj["C", "End"] <- 1
net <- CographNetwork$new(adj, directed = TRUE)
layout_saqr(net)
#>           x         y
#> 1 0.5000000 1.0000000
#> 2 0.0669873 0.7733333
#> 3 0.9330127 0.5600000
#> 4 0.5000000 0.3333333
#> 5 0.5000000 0.0000000
```
