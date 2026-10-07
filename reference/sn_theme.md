# Apply Theme to Network

Apply a visual theme to the network.

## Usage

``` r
sn_theme(network, theme, ...)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- theme:

  Theme name (string) or CographTheme object.

- ...:

  Theme parameters to override, such as `background`, `node_fill` or
  `edge_color`. An unknown parameter name raises an error.

## Value

The `cograph_network` with the theme stored in its `theme` element.

## Details

### Available Themes

- `"classic"`:

  White background, blue nodes and gray edges.

- `"dark"`:

  Dark background with bright nodes, for presentations.

- `"minimal"`:

  Subtle styling with thin edges and muted colors.

- `"colorblind"`:

  Optimized for color vision deficiency.

- `"gray"`/`"grey"`:

  Black and white theme suitable for print.

- `"viridis"`:

  Perceptually uniform colors.

- `"nature"`:

  Nature-inspired colors.

Use [`list_themes()`](https://sonsoles.me/cograph/reference/themes.md)
to see all available themes.

## See also

[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
network creation,
[`sn_palette`](https://sonsoles.me/cograph/reference/sn_palette.md) for
color palettes,
[`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`sn_edges`](https://sonsoles.me/cograph/reference/sn_edges.md) for edge
customization,
[`list_themes`](https://sonsoles.me/cograph/reference/themes.md) to see
available themes,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for plotting

## Examples

``` r
cograph(regulation_net) |> sn_theme("dark") |> splot()
```
