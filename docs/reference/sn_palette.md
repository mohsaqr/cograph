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
[`list_palettes()`](http://sonsoles.me/cograph/reference/list_palettes.md)
to see all available palettes. Common options:

- **viridis**:

  Perceptually uniform, colorblind-friendly.

- **colorblind**:

  Optimized for color vision deficiency.

- **pastel**:

  Soft, muted colors.

- **bright**:

  Saturated, vivid colors.

- **grayscale**:

  Shades of gray.

You can also pass a custom palette function that takes `n` and returns
`n` colors.

## See also

[`cograph`](http://sonsoles.me/cograph/reference/cograph.md) for network
creation, [`sn_theme`](http://sonsoles.me/cograph/reference/sn_theme.md)
for visual themes,
[`sn_nodes`](http://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`list_palettes`](http://sonsoles.me/cograph/reference/list_palettes.md)
to see available palettes,
[`splot`](http://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](http://sonsoles.me/cograph/reference/soplot.md) for plotting

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Apply palette to nodes
cograph(adj) |> sn_palette("viridis") |> splot()


# Apply to edges
cograph(adj) |> sn_palette("colorblind", target = "edges") |> splot()

# Apply to both
cograph(adj) |> sn_palette("pastel", target = "both") |> splot()

# Custom palette function
my_pal <- function(n) rainbow(n, s = 0.7)
cograph(adj) |> sn_palette(my_pal) |> splot()

# Direct matrix input
adj |> sn_palette("viridis")
#> Cograph network: 3 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): 1, 2, 3
#>   Edges: 3 / 3 (density: 100.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     1 -- 2  1.000
#>     1 -- 3  1.000
#>     2 -- 3  1.000
#> Layout: set 
```
