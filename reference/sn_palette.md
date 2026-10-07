# Apply Color Palette to Network

Apply a color palette for node and/or edge coloring.

## Usage

``` r
sn_palette(network, palette, target = "nodes", by = NULL)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- palette:

  Palette name (see
  [`list_palettes`](https://sonsoles.me/cograph/reference/palettes.md))
  or a function that takes `n` and returns `n` colors.

- target:

  What to apply the palette to: "nodes" (default), "edges", or "both".
  For edges, the first two palette colors become the colors of positive
  and negative edges.

- by:

  Name of a node-table column whose values are mapped to palette colors.
  When `by` is NULL or not a column of the node table, every node gets
  the first palette color.

## Value

The `cograph_network` with the node fill colors and edge colors stored
in its `node_aes` and `edge_aes` elements.

## Details

### Available Palettes

Use
[`list_palettes()`](https://sonsoles.me/cograph/reference/palettes.md)
to see all available palettes. Common options:

- `"viridis"`:

  Perceptually uniform, colorblind-friendly.

- `"colorblind"`:

  Optimized for color vision deficiency.

- `"pastel"`:

  Soft, muted colors.

- `"blues"`:

  Blue sequential palette.

- `"reds"`:

  Red sequential palette.

- `"diverging"`:

  Blue-white-red diverging palette.

## See also

[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
network creation,
[`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md) for
visual themes,
[`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`list_palettes`](https://sonsoles.me/cograph/reference/palettes.md) to
see available palettes,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for plotting

## Examples

``` r
cograph(regulation_net) |> sn_palette("viridis") |> splot()
```
