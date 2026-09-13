# Network Wrangling Verbs

cograph's verbs for reshaping a network. Every verb takes any supported
input (matrix, edge list, igraph, statnet network, tna model,
`cograph_network`), takes its options as named arguments, and returns a
`cograph_network` — or the input format when `keep_format = TRUE`. There
is no pipeline state to activate and nothing to unpack afterwards: use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for the
tidy edge or node table.

## Value

Each verb returns a `cograph_network`, except
[`split_components()`](https://sonsoles.me/cograph/reference/split_components.md),
which returns a list of them. With `keep_format = TRUE` a matrix,
igraph, statnet network or tna input comes back in that format.

## Selecting

- [`filter_nodes()`](https://sonsoles.me/cograph/reference/filter_nodes.md),
  [`select_nodes()`](https://sonsoles.me/cograph/reference/select_nodes.md):

  Keep nodes by expression, name, index, top-N, neighborhood or
  component.

- [`filter_edges()`](https://sonsoles.me/cograph/reference/filter_edges.md),
  [`select_edges()`](https://sonsoles.me/cograph/reference/select_edges.md):

  Keep edges by expression, endpoints, bridges, mutuality or top-N.

- [`select_neighbors()`](https://sonsoles.me/cograph/reference/select_neighbors.md),
  [`select_component()`](https://sonsoles.me/cograph/reference/select_component.md),
  [`select_top()`](https://sonsoles.me/cograph/reference/select_top.md),
  [`select_k_core()`](https://sonsoles.me/cograph/reference/select_k_core.md):

  Named shorthands for the common selections.

- [`split_components()`](https://sonsoles.me/cograph/reference/split_components.md):

  One network per connected component.

## Weights

- [`threshold_edges()`](https://sonsoles.me/cograph/reference/threshold_edges.md):

  Keep edges by weight, count, proportion or density.

- [`binarize()`](https://sonsoles.me/cograph/reference/binarize.md):

  Replace weights with 0/1.

- [`symmetrize()`](https://sonsoles.me/cograph/reference/symmetrize.md):

  Combine opposite arcs into one edge.

- [`normalize_weights()`](https://sonsoles.me/cograph/reference/normalize_weights.md):

  Rescale by row, column, maximum, total, or to \[0, 1\].

- [`invert_weights()`](https://sonsoles.me/cograph/reference/invert_weights.md):

  Turn similarities into distances.

## Structure

- [`to_undirected()`](https://sonsoles.me/cograph/reference/to_undirected.md),
  [`to_directed()`](https://sonsoles.me/cograph/reference/to_directed.md),
  [`reverse_edges()`](https://sonsoles.me/cograph/reference/reverse_edges.md):

  Change directedness.

- [`remove_isolates()`](https://sonsoles.me/cograph/reference/remove_isolates.md):

  Drop nodes with no edges.

- [`contract_nodes()`](https://sonsoles.me/cograph/reference/contract_nodes.md):

  Collapse groups of nodes into one.

- [`spanning_tree()`](https://sonsoles.me/cograph/reference/spanning_tree.md),
  [`complement_network()`](https://sonsoles.me/cograph/reference/complement_network.md):

  Derived graphs.

- [`reorder_nodes()`](https://sonsoles.me/cograph/reference/reorder_nodes.md),
  [`rename_nodes()`](https://sonsoles.me/cograph/reference/rename_nodes.md):

  Change node order or labels without changing the network.

- [`simplify()`](https://sonsoles.me/cograph/reference/simplify.md):

  Merge duplicate edges and drop loops.

## Editing

- [`add_nodes()`](https://sonsoles.me/cograph/reference/add_nodes.md),
  [`remove_nodes()`](https://sonsoles.me/cograph/reference/remove_nodes.md),
  [`add_edges()`](https://sonsoles.me/cograph/reference/add_edges.md),
  [`remove_edges()`](https://sonsoles.me/cograph/reference/remove_edges.md):

  Add and remove.

- [`mutate_nodes()`](https://sonsoles.me/cograph/reference/mutate_nodes.md),
  [`mutate_edges()`](https://sonsoles.me/cograph/reference/mutate_edges.md):

  Compute and store attributes.

- [`bind_networks()`](https://sonsoles.me/cograph/reference/bind_networks.md):

  Union, intersection or difference of two networks.

## Conversion and access

[`as_cograph()`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`to_matrix()`](https://sonsoles.me/cograph/reference/to_matrix.md),
[`to_igraph()`](https://sonsoles.me/cograph/reference/to_igraph.md),
[`to_network()`](https://sonsoles.me/cograph/reference/to_network.md),
[`to_df()`](https://sonsoles.me/cograph/reference/to_data_frame.md), and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
`cograph_network` (see
[`as.data.frame.cograph_network`](https://sonsoles.me/cograph/reference/as.data.frame.cograph_network.md)).

## Semantics worth knowing

- **Filtering edges does not remove nodes.** This matches
  [`igraph::delete_edges()`](https://r.igraph.org/reference/delete_edges.html)
  and tidygraph. Nodes left without edges raise a
  `cograph_isolates_created` warning; call
  [`remove_isolates()`](https://sonsoles.me/cograph/reference/remove_isolates.md)
  to drop them, or pass `keep_isolates = FALSE`.

- **Undirected results stay undirected.** The weight matrix of an
  undirected result is symmetric, so nothing downstream re-detects it as
  directed.

- **Metadata survives.** Node groups, estimation data, layout
  coordinates and the original source type are carried through every
  verb.

- **Malformed selections are errors.** Unknown node names, out-of- range
  or fractional indices, unknown measure names and a malformed `between`
  raise a `cograph_bad_selection` error rather than warning and
  returning something plausible.

## Related verbs elsewhere

[`ego_networks()`](https://sonsoles.me/cograph/reference/ego_networks.md),
[`shortest_paths()`](https://sonsoles.me/cograph/reference/shortest_paths.md),
[`disparity_filter()`](https://sonsoles.me/cograph/reference/disparity_filter.md),
[`detect_communities()`](https://sonsoles.me/cograph/reference/detect_communities.md),
[`summarize_clusters()`](https://sonsoles.me/cograph/reference/summarize_clusters.md),
[`aggregate_layers()`](https://sonsoles.me/cograph/reference/aggregate_layers.md).

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# One call, named arguments, a tidy table out
as.data.frame(threshold_edges(adj, minimum = 0.4))
#>   from to weight
#> 1    A  B    0.5
#> 2    A  C    0.8
#> 3    B  D    0.6
#> 4    C  D    0.4

# Verbs compose
adj |>
  threshold_edges(minimum = 0.4) |>
  remove_isolates() |>
  mutate_nodes(deg = degree) |>
  as.data.frame(what = "nodes")
#>   id label name  x  y deg
#> 1  1     A    A NA NA   2
#> 2  2     B    B NA NA   2
#> 3  3     C    C NA NA   2
#> 4  4     D    D NA NA   2
```
