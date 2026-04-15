# Convert to Cograph Network

Creates a lightweight cograph_network object from various network
inputs. The resulting object is a named list with all data accessible
via `$`.

## Usage

``` r
as_cograph(x, directed = NULL, simplify = FALSE, ...)

to_cograph(x, directed = NULL, ...)
```

## Arguments

- x:

  Network input. Can be:

  - A square numeric matrix (adjacency/weight matrix)

  - A data frame with edge list (from, to, optional weight columns)

  - An igraph object

  - A statnet network object

  - A qgraph object

  - A tna object

  - An existing cograph_network object (returned as-is)

- directed:

  Logical. Force directed interpretation. NULL for auto-detect.

- simplify:

  Logical or character. If FALSE (default), every transition from tna
  sequence data is a separate edge. If TRUE or a string ("sum", "mean",
  "max", "min"), duplicate edges are aggregated.

- ...:

  Additional arguments (currently unused).

## Value

A cograph_network object: a named list with components:

- `nodes`:

  Data frame with id, label, (x, y if layout applied)

- `edges`:

  Data frame with from, to, weight columns

- `directed`:

  Logical indicating if network is directed

- `weights`:

  Full n×n weight matrix (for to_matrix round-trip)

- `data`:

  Original estimation data (sequence matrix, edge list, etc.), or NULL

- `meta`:

  Consolidated metadata list with sub-fields: `source` (input type
  string), `layout` (layout info list or NULL), `tna` (TNA metadata or
  NULL)

- `node_groups`:

  Optional node groupings data frame

A `cograph_network` object. See `as_cograph`.

## Details

The cograph_network format is designed to be:

- Lean: Only essential data stored, computed values derived on demand

- Modern: Uses named list elements instead of attributes for clean `$`
  access

- Compatible: Works seamlessly with splot() and other cograph functions

Use getter functions for programmatic access:
[`get_nodes`](https://sonsoles.me/cograph/reference/get_nodes.md),
[`get_edges`](https://sonsoles.me/cograph/reference/get_edges.md),
[`get_labels`](https://sonsoles.me/cograph/reference/get_labels.md),
[`n_nodes`](https://sonsoles.me/cograph/reference/n_nodes.md),
[`n_edges`](https://sonsoles.me/cograph/reference/n_edges.md)

Use setter functions to modify:
[`set_nodes`](https://sonsoles.me/cograph/reference/set_nodes.md),
[`set_edges`](https://sonsoles.me/cograph/reference/set_edges.md),
[`set_layout`](https://sonsoles.me/cograph/reference/set_layout.md)

## See also

[`get_nodes`](https://sonsoles.me/cograph/reference/get_nodes.md) to
extract the nodes data frame,
[`get_edges`](https://sonsoles.me/cograph/reference/get_edges.md) to
extract edges as a data frame,
[`n_nodes`](https://sonsoles.me/cograph/reference/n_nodes.md) and
[`n_edges`](https://sonsoles.me/cograph/reference/n_edges.md) for
counts,
[`is_directed`](https://sonsoles.me/cograph/reference/is_directed.md) to
check directedness,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) for plotting

## Examples

``` r
# From adjacency matrix
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)

# Direct $ access to core data
net$nodes      # nodes data frame
#>   id label name  x  y
#> 1  1     1    1 NA NA
#> 2  2     2    2 NA NA
#> 3  3     3    3 NA NA
net$edges      # edges data frame
#>   from to weight
#> 1    1  2      1
#> 2    1  3      1
#> 3    2  3      1
net$directed   # TRUE/FALSE
#> [1] FALSE

# Getter functions (recommended for programmatic use)
get_nodes(net)   # nodes data frame
#>   id label name  x  y
#> 1  1     1    1 NA NA
#> 2  2     2    2 NA NA
#> 3  3     3    3 NA NA
get_edges(net)   # edges data frame (from, to, weight)
#>   from to weight
#> 1    1  2      1
#> 2    1  3      1
#> 3    2  3      1
get_labels(net)  # character vector of labels
#> [1] "1" "2" "3"
n_nodes(net)     # 3
#> [1] 3
n_edges(net)     # 3
#> [1] 3
cograph::is_directed(net) # FALSE (symmetric matrix)
#> [1] FALSE

# Setter functions
net <- set_nodes(net, data.frame(id = 1:3, label = c("A", "B", "C")))
net <- set_edges(net, data.frame(from = c(1,2), to = c(2,3), weight = c(0.5, 0.8)))
net <- set_layout(net, data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))

# Plot it
splot(net)


# From igraph (if installed)
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_ring(10)
  net <- as_cograph(g)
  splot(net)
}

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- to_cograph(mat)
```
