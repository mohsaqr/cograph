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

  Palette name or function.

- target:

  What to apply the palette to: "nodes", "edges", or "both".

- by:

  Variable to map colors to (for nodes: column name or "group").

## Value

Modified cograph_network object.

## Details

### Available Palettes

Use
[`list_palettes()`](https://sonsoles.me/cograph/reference/list_palettes.md)
to see all available palettes. Common options:

- **viridis**:

  Perceptually uniform, colorblind-friendly.

- **colorblind**:

  Optimized for color vision deficiency.

- **pastel**:

  Soft, muted colors.

- **blues**:

  Blue sequential palette.

- **reds**:

  Red sequential palette.

- **diverging**:

  Blue-white-red diverging palette.

You can also pass a custom palette function that takes `n` and returns
`n` colors.

## See also

[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
network creation,
[`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md) for
visual themes,
[`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`list_palettes`](https://sonsoles.me/cograph/reference/list_palettes.md)
to see available palettes,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for plotting

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
cograph(adj) |> sn_palette("viridis") |> splot()


# Apply to edges
cograph(adj) |> sn_palette("colorblind", target = "edges") |> splot()
```
