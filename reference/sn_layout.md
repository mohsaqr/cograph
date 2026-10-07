# Apply Layout to Network

Apply a layout algorithm to compute node positions.

## Usage

``` r
sn_layout(network, layout, seed = 42, ...)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- layout:

  Layout algorithm name (see Details), a two-letter or full igraph
  layout name, an igraph layout function, a `CographLayout` object, or a
  coordinate matrix / data frame with one row per node and `x`, `y` in
  its first two columns. Anything else is an error.

- seed:

  Random seed for deterministic layouts. Default 42. Set NULL for
  random.

- ...:

  Additional arguments passed to the layout function.

## Value

The `cograph_network` with node coordinates in the `x` and `y` columns
of its node table.

## Details

### Built-in Layouts

- `"spring"`:

  Force-directed layout (Fruchterman-Reingold).

- `"oval"`/`"ellipse"`:

  Nodes arranged around an ellipse.

- `"circle"`:

  Nodes arranged in a circle.

- `"groups"`:

  Circular layout with grouped nodes clustered together.

- `"grid"`:

  Nodes in a regular grid.

- `"random"`:

  Random positions.

- `"star"`:

  Central node with others arranged around it.

- `"bipartite"`:

  Two-column layout for bipartite networks.

- `"gephi"`/`"gephi_fr"`:

  Gephi-style force-directed layout.

### igraph Layouts

Two-letter codes for igraph layouts: "kk" (Kamada-Kawai), "fr"
(Fruchterman-Reingold), "drl", "mds", "ni" (nicely), "tr" (tree), "ci"
(circle), etc.

igraph layout functions and full igraph layout names such as
`"layout_with_kk"` are also accepted.

## See also

[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
network creation,
[`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`sn_edges`](https://sonsoles.me/cograph/reference/sn_edges.md) for edge
customization,
[`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md) for
visual themes, [`splot`](https://sonsoles.me/cograph/reference/splot.md)
and [`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for
plotting

## Examples

``` r
cograph(regulation_net) |> sn_layout("circle") |> splot()
```
