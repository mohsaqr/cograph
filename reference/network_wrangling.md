# Network Wrangling Verbs

cograph provides verbs for reshaping a network. Each verb accepts any
supported input (matrix, edge list, igraph, statnet network, tna model,
`cograph_network`) and takes its options as named arguments. The result
is a `cograph_network`, or the input format when `keep_format = TRUE`.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the edge or node table of a result.

## Value

Each verb returns a `cograph_network`, except
[`split_components()`](https://sonsoles.me/cograph/reference/split_components.md),
which returns a list of them. With `keep_format = TRUE` a matrix,
igraph, statnet network or tna input comes back in that format. An
edge-list data frame comes back as a `cograph_network` with a warning.

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

  Merge duplicate edges and drop loops. It returns the input format.

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
[`as.data.frame.cograph_network`](https://sonsoles.me/cograph/reference/as_cograph.md)).

## Semantics

- Filtering edges does not remove nodes, as in
  [`igraph::delete_edges()`](https://r.igraph.org/reference/delete_edges.html)
  and tidygraph. Nodes left without edges raise a
  `cograph_isolates_created` warning. They are dropped by
  [`remove_isolates()`](https://sonsoles.me/cograph/reference/remove_isolates.md)
  or by `keep_isolates = FALSE` in the verbs that have that argument.

- The weight matrix of an undirected result is symmetric, so the result
  is still detected as undirected downstream.

- Node groups, estimation data, layout coordinates and the original
  source type are carried through the verbs.

- Unknown node names, out-of-range or fractional indices, unknown
  measure names and a malformed `between` raise a
  `cograph_bad_selection` error. The `name` argument of
  [`select_nodes()`](https://sonsoles.me/cograph/reference/select_nodes.md)
  is an exception. Unknown names in it are skipped, and a warning is
  given when no node matches.

## Related verbs elsewhere

[`ego_networks()`](https://sonsoles.me/cograph/reference/ego_networks.md),
[`shortest_paths()`](https://sonsoles.me/cograph/reference/shortest_paths.md),
[`disparity_filter()`](https://sonsoles.me/cograph/reference/disparity_filter.md),
[`detect_communities()`](https://sonsoles.me/cograph/reference/detect_communities.md),
[`summarize_clusters()`](https://sonsoles.me/cograph/reference/summarize_clusters.md),
[`aggregate_layers()`](https://sonsoles.me/cograph/reference/aggregate_layers.md).

## Examples

``` r
as.data.frame(threshold_edges(regulation_net, minimum = 0.2))
#>          from       to weight
#> 1       Adapt  Explore   0.28
#> 2     Discuss  Explore   0.30
#> 3       Share     Plan   0.21
#> 4    Evaluate  Monitor   0.33
#> 5       Share  Monitor   0.49
#> 6    Evaluate    Adapt   0.43
#> 7       Share    Adapt   0.39
#> 8     Explore  Reflect   0.35
#> 9     Discuss  Reflect   0.35
#> 10 Synthesize  Reflect   0.42
#> 11       Plan  Discuss   0.40
#> 12      Adapt  Discuss   0.34
#> 13       Plan Evaluate   0.49
#> 14     Create Evaluate   0.39
#> 15       Plan   Create   0.20
#> 16    Monitor   Create   0.37
#> 17    Explore    Share   0.27
#> 18       Plan    Share   0.36
#> 19     Create    Share   0.23
```
