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

  Layout algorithm name or a CographLayout object.

- seed:

  Random seed for deterministic layouts. Default 42. Set NULL for
  random.

- ...:

  Additional arguments passed to the layout function.

## Value

Modified cograph_network object.

## Details

### Built-in Layouts

- **spring**:

  Force-directed layout (Fruchterman-Reingold style). Good
  general-purpose layout. Default.

- **circle**:

  Nodes arranged in a circle. Good for small networks or when structure
  is less important.

- **groups**:

  Circular layout with grouped nodes clustered together.

- **grid**:

  Nodes in a regular grid.

- **random**:

  Random positions. Useful as starting point.

- **star**:

  Central node with others arranged around it.

- **bipartite**:

  Two-column layout for bipartite networks.

### igraph Layouts

Two-letter codes for igraph layouts: "kk" (Kamada-Kawai), "fr"
(Fruchterman-Reingold), "drl", "mds", "ni" (nicely), "tr" (tree), "ci"
(circle), etc.

You can also pass igraph layout functions directly or use full names
like "layout_with_kk".

## See also

[`cograph`](http://sonsoles.me/cograph/reference/cograph.md) for network
creation, [`sn_nodes`](http://sonsoles.me/cograph/reference/sn_nodes.md)
for node customization,
[`sn_edges`](http://sonsoles.me/cograph/reference/sn_edges.md) for edge
customization,
[`sn_theme`](http://sonsoles.me/cograph/reference/sn_theme.md) for
visual themes, [`splot`](http://sonsoles.me/cograph/reference/splot.md)
and [`soplot`](http://sonsoles.me/cograph/reference/soplot.md) for
plotting

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Built-in layouts
cograph(adj) |> sn_layout("circle") |> splot()

cograph(adj) |> sn_layout("spring") |> splot()


# igraph layouts (if igraph installed)
if (requireNamespace("igraph", quietly = TRUE)) {
  cograph(adj) |> sn_layout("kk") |> splot()
  cograph(adj) |> sn_layout("fr") |> splot()
}



# Custom coordinates
coords <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
cograph(adj) |> sn_layout(coords) |> splot()


# Direct matrix input (auto-converts)
adj |> sn_layout("circle")
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
