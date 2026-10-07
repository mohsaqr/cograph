# CographNetwork R6 Class

Core class representing a network for visualization. Stores nodes,
edges, layout coordinates, and aesthetic mappings.

## Value

A `CographNetwork` R6 object.

## Active bindings

- `n_nodes`:

  Number of nodes in the network.

- `n_edges`:

  Number of edges in the network.

- `is_directed`:

  Whether the network is directed.

- `has_weights`:

  `TRUE` when any edge weight differs from 1.

- `node_labels`:

  Vector of node labels, taken from the `labels` column of the node
  table when present and from `label` otherwise.

## Methods

### Public methods

- [`CographNetwork$new()`](#method-CographNetwork-new)

- [`CographNetwork$clone_network()`](#method-CographNetwork-clone_network)

- [`CographNetwork$set_nodes()`](#method-CographNetwork-set_nodes)

- [`CographNetwork$set_edges()`](#method-CographNetwork-set_edges)

- [`CographNetwork$set_directed()`](#method-CographNetwork-set_directed)

- [`CographNetwork$set_weights()`](#method-CographNetwork-set_weights)

- [`CographNetwork$set_layout_coords()`](#method-CographNetwork-set_layout_coords)

- [`CographNetwork$set_node_aes()`](#method-CographNetwork-set_node_aes)

- [`CographNetwork$set_edge_aes()`](#method-CographNetwork-set_edge_aes)

- [`CographNetwork$set_theme()`](#method-CographNetwork-set_theme)

- [`CographNetwork$get_nodes()`](#method-CographNetwork-get_nodes)

- [`CographNetwork$get_edges()`](#method-CographNetwork-get_edges)

- [`CographNetwork$get_layout()`](#method-CographNetwork-get_layout)

- [`CographNetwork$get_node_aes()`](#method-CographNetwork-get_node_aes)

- [`CographNetwork$get_edge_aes()`](#method-CographNetwork-get_edge_aes)

- [`CographNetwork$get_theme()`](#method-CographNetwork-get_theme)

- [`CographNetwork$set_layout_info()`](#method-CographNetwork-set_layout_info)

- [`CographNetwork$get_layout_info()`](#method-CographNetwork-get_layout_info)

- [`CographNetwork$set_plot_params()`](#method-CographNetwork-set_plot_params)

- [`CographNetwork$get_plot_params()`](#method-CographNetwork-get_plot_params)

- [`CographNetwork$print()`](#method-CographNetwork-print)

- [`CographNetwork$clone()`](#method-CographNetwork-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CographNetwork object.

#### Usage

    CographNetwork$new(
      input = NULL,
      directed = NULL,
      nodes = NULL,
      simplify = FALSE
    )

#### Arguments

- `input`:

  Network input, such as a matrix, edge list, igraph, statnet network,
  qgraph or tna object. `NULL` creates an empty object.

- `directed`:

  Logical. Forces a directed or undirected interpretation. `NULL`
  detects it from the input.

- `nodes`:

  `NULL` or a data frame of node attributes. Its rows are matched to the
  node labels by a `name`, `label` or `id` column, tried in that order,
  and the remaining columns are added to the node table. When no column
  matches and the row count equals the number of nodes, the columns are
  added in row order. A `labels` column supplies the display labels
  returned by `$node_labels`.

- `simplify`:

  Logical or character. If FALSE (default), every transition from tna
  sequence data is a separate edge. If TRUE or a string ("sum", "mean",
  "max", "min"), duplicate transitions are aggregated. Other inputs are
  not affected.

#### Returns

A new CographNetwork object.

------------------------------------------------------------------------

### Method `clone_network()`

Create a copy with the same nodes, edges, weights, layout, aesthetics,
theme, layout information and plot parameters.

#### Usage

    CographNetwork$clone_network()

#### Returns

A new CographNetwork object.

------------------------------------------------------------------------

### Method [`set_nodes()`](https://sonsoles.me/cograph/reference/get_nodes.md)

Set nodes data frame.

#### Usage

    CographNetwork$set_nodes(nodes)

#### Arguments

- `nodes`:

  Data frame with node information.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method [`set_edges()`](https://sonsoles.me/cograph/reference/get_nodes.md)

Set edges data frame.

#### Usage

    CographNetwork$set_edges(edges)

#### Arguments

- `edges`:

  Data frame with edge information.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `set_directed()`

Set directed flag.

#### Usage

    CographNetwork$set_directed(directed)

#### Arguments

- `directed`:

  Logical.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `set_weights()`

Set edge weights.

#### Usage

    CographNetwork$set_weights(weights)

#### Arguments

- `weights`:

  Numeric vector of edge weights, one per edge.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `set_layout_coords()`

Set layout coordinates.

#### Usage

    CographNetwork$set_layout_coords(coords)

#### Arguments

- `coords`:

  Matrix or data frame with at least two columns and one row per node.
  The first two columns are renamed `x` and `y` and are also written to
  the node table. `NULL` leaves the layout unchanged.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `set_node_aes()`

Set node aesthetics. The list is merged into the current node
aesthetics.

#### Usage

    CographNetwork$set_node_aes(aes)

#### Arguments

- `aes`:

  Named list of aesthetic parameters.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `set_edge_aes()`

Set edge aesthetics. The list is merged into the current edge
aesthetics.

#### Usage

    CographNetwork$set_edge_aes(aes)

#### Arguments

- `aes`:

  Named list of aesthetic parameters.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `set_theme()`

Set theme.

#### Usage

    CographNetwork$set_theme(theme)

#### Arguments

- `theme`:

  CographTheme object or theme name.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method [`get_nodes()`](https://sonsoles.me/cograph/reference/get_nodes.md)

Get nodes data frame.

#### Usage

    CographNetwork$get_nodes()

#### Returns

Data frame with node information.

------------------------------------------------------------------------

### Method [`get_edges()`](https://sonsoles.me/cograph/reference/get_nodes.md)

Get edges data frame.

#### Usage

    CographNetwork$get_edges()

#### Returns

Data frame with edge information.

------------------------------------------------------------------------

### Method [`get_layout()`](https://sonsoles.me/cograph/reference/layout_registry.md)

Get layout coordinates.

#### Usage

    CographNetwork$get_layout()

#### Returns

A data frame with `x` and `y` columns, or `NULL` when no layout is set.

------------------------------------------------------------------------

### Method `get_node_aes()`

Get node aesthetics.

#### Usage

    CographNetwork$get_node_aes()

#### Returns

List of node aesthetic parameters.

------------------------------------------------------------------------

### Method `get_edge_aes()`

Get edge aesthetics.

#### Usage

    CographNetwork$get_edge_aes()

#### Returns

List of edge aesthetic parameters.

------------------------------------------------------------------------

### Method [`get_theme()`](https://sonsoles.me/cograph/reference/themes.md)

Get theme.

#### Usage

    CographNetwork$get_theme()

#### Returns

The stored theme (a CographTheme object or theme name), or `NULL`.

------------------------------------------------------------------------

### Method `set_layout_info()`

Set layout info.

#### Usage

    CographNetwork$set_layout_info(info)

#### Arguments

- `info`:

  List with layout information (name, seed, etc.).

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `get_layout_info()`

Get layout info.

#### Usage

    CographNetwork$get_layout_info()

#### Returns

List with layout information.

------------------------------------------------------------------------

### Method `set_plot_params()`

Set plot parameters.

#### Usage

    CographNetwork$set_plot_params(params)

#### Arguments

- `params`:

  List of all plot parameters used.

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `get_plot_params()`

Get plot parameters.

#### Usage

    CographNetwork$get_plot_params()

#### Returns

List of plot parameters.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print network summary.

#### Usage

    CographNetwork$print()

#### Returns

The object itself, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CographNetwork$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
CographNetwork$new(regulation_net)
#> CographNetwork
#>   Nodes: 10 
#>   Edges: 30 
#>   Directed: TRUE 
#>   Layout: none 
```
