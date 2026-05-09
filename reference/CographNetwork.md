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

  Whether edges have weights.

- `node_labels`:

  Vector of node labels (priority: labels \> label).

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

  Network input supported by
  [`parse_input`](https://sonsoles.me/cograph/reference/parse_input.md),
  such as a matrix, edge list, igraph, statnet network, qgraph, or tna
  object.

- `directed`:

  Logical. Force directed interpretation. NULL for auto-detect.

- `nodes`:

  Node metadata. Can be NULL or a data frame with node attributes. If
  data frame has a `label` or `labels` column, those are used for
  display.

- `simplify`:

  Logical or character. If FALSE (default), every transition from tna
  sequence data is a separate edge. If TRUE or a string ("sum", "mean",
  "max", "min"), duplicate edges are aggregated.

#### Returns

A new CographNetwork object.

------------------------------------------------------------------------

### Method `clone_network()`

Clone the network with optional modifications.

#### Usage

    CographNetwork$clone_network()

#### Returns

A new CographNetwork object.

------------------------------------------------------------------------

### Method [`set_nodes()`](https://sonsoles.me/cograph/reference/set_nodes.md)

Set nodes data frame.

#### Usage

    CographNetwork$set_nodes(nodes)

#### Arguments

- `nodes`:

  Data frame with node information.

------------------------------------------------------------------------

### Method [`set_edges()`](https://sonsoles.me/cograph/reference/set_edges.md)

Set edges data frame.

#### Usage

    CographNetwork$set_edges(edges)

#### Arguments

- `edges`:

  Data frame with edge information.

------------------------------------------------------------------------

### Method `set_directed()`

Set directed flag.

#### Usage

    CographNetwork$set_directed(directed)

#### Arguments

- `directed`:

  Logical.

------------------------------------------------------------------------

### Method `set_weights()`

Set edge weights.

#### Usage

    CographNetwork$set_weights(weights)

#### Arguments

- `weights`:

  Numeric vector of weights.

------------------------------------------------------------------------

### Method `set_layout_coords()`

Set layout coordinates.

#### Usage

    CographNetwork$set_layout_coords(coords)

#### Arguments

- `coords`:

  Matrix or data frame with x, y columns.

------------------------------------------------------------------------

### Method `set_node_aes()`

Set node aesthetics.

#### Usage

    CographNetwork$set_node_aes(aes)

#### Arguments

- `aes`:

  List of aesthetic parameters.

------------------------------------------------------------------------

### Method `set_edge_aes()`

Set edge aesthetics.

#### Usage

    CographNetwork$set_edge_aes(aes)

#### Arguments

- `aes`:

  List of aesthetic parameters.

------------------------------------------------------------------------

### Method `set_theme()`

Set theme.

#### Usage

    CographNetwork$set_theme(theme)

#### Arguments

- `theme`:

  CographTheme object or theme name.

------------------------------------------------------------------------

### Method [`get_nodes()`](https://sonsoles.me/cograph/reference/get_nodes.md)

Get nodes data frame.

#### Usage

    CographNetwork$get_nodes()

#### Returns

Data frame with node information.

------------------------------------------------------------------------

### Method [`get_edges()`](https://sonsoles.me/cograph/reference/get_edges.md)

Get edges data frame.

#### Usage

    CographNetwork$get_edges()

#### Returns

Data frame with edge information.

------------------------------------------------------------------------

### Method [`get_layout()`](https://sonsoles.me/cograph/reference/get_layout.md)

Get layout coordinates.

#### Usage

    CographNetwork$get_layout()

#### Returns

Data frame with x, y coordinates.

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

### Method [`get_theme()`](https://sonsoles.me/cograph/reference/get_theme.md)

Get theme.

#### Usage

    CographNetwork$get_theme()

#### Returns

CographTheme object.

------------------------------------------------------------------------

### Method `set_layout_info()`

Set layout info.

#### Usage

    CographNetwork$set_layout_info(info)

#### Arguments

- `info`:

  List with layout information (name, seed, etc.).

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
# Create network from adjacency matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)

# Access properties
net$n_nodes
#> [1] 3
net$n_edges
#> [1] 3
net$is_directed
#> [1] FALSE
```
