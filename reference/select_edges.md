# Select Edges

Selects edges by filter expressions, by node sets, by rank on a metric,
or by structural properties such as bridges, communities and
reciprocity.

## Usage

``` r
select_edges(
  x,
  ...,
  top = NULL,
  by = "weight",
  involving = NULL,
  between = NULL,
  bridges_only = FALSE,
  mutual_only = FALSE,
  community = "louvain",
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL,
  .keep_isolates = NULL
)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, or tna
  object.

- ...:

  Filter expressions using edge columns or computed metrics. Available
  variables:

  Edge columns

  :   `from`, `to`, `weight`, plus any custom

  Computed metrics

  :   `abs_weight`, `from_degree`, `to_degree`, `from_strength`,
      `to_strength`, `edge_betweenness`, `weight_rank`

  Predicates

  :   `is_bridge`, `is_mutual` (alias `is_reciprocal`), `is_loop`,
      `is_multiple`, `same_community`

  Endpoint labels

  :   `from_label`, `to_label`, `from_community`, `to_community`

- top:

  Integer. Select top N edges by a metric.

- by:

  Character. Metric for top selection. Default `"weight"`. Options:
  `"weight"`, `"abs_weight"`, `"edge_betweenness"`, `"from_degree"`,
  `"to_degree"`, `"from_strength"`, `"to_strength"`, `"weight_rank"`.

- involving:

  Character or integer. Select edges involving these nodes (by name or
  index). An edge is selected if either endpoint matches.

- between:

  List of two character/integer vectors. Select edges between two node
  sets. Example: `between = list(c("A", "B"), c("C", "D"))`.

- bridges_only:

  Logical. Select only bridge edges (edges whose removal disconnects the
  graph). Default FALSE.

- mutual_only:

  Logical. For directed networks, select only mutual (reciprocated)
  edges. Default FALSE.

- community:

  Character. Community detection method for `same_community` variable.
  One of `"louvain"`, `"walktrap"`, `"fast_greedy"`, `"label_prop"`,
  `"infomap"`, `"leiden"`. Default `"louvain"`.

- keep_isolates:

  Logical. Keep nodes that end up with no edges? Default TRUE, matching
  [`igraph::delete_edges()`](https://r.igraph.org/reference/delete_edges.html)
  and tidygraph: filtering edges does not remove nodes. Set FALSE to
  drop them, or call
  [`remove_isolates()`](https://sonsoles.me/cograph/reference/remove_isolates.md)
  afterwards.

- keep_format:

  Logical. If TRUE, matrix, igraph, statnet network and tna inputs are
  returned in that format. Default FALSE returns cograph_network.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

- .keep_isolates:

  Deprecated. Use `keep_isolates`.

## Value

A cograph_network object with selected edges. If `keep_format = TRUE`,
matrix, igraph, statnet network and tna inputs are converted back to
that type. Nodes left without edges are kept and reported in a
`cograph_isolates_created` warning, unless `keep_isolates = FALSE`.

## Details

Selection criteria are combined with AND logic, so an edge is selected
only when it satisfies all of them. The `top` ranking is applied to the
edges that pass `involving`, `between`, `bridges_only` and
`mutual_only`, and the filter expressions in `...` are applied
afterwards. For example, `select_edges(x, top = 10, involving = "A")`
selects the 10 strongest edges among those involving node A.

Only the edge metrics referenced in expressions or required by a
selection criterion are computed.

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`select_bridges`](https://sonsoles.me/cograph/reference/select_bridges.md),
[`select_top_edges`](https://sonsoles.me/cograph/reference/select_top_edges.md)

## Examples

``` r
select_edges(regulation_net, top = 5, keep_isolates = FALSE)
#> Cograph network: 8 nodes, 5 edges ( directed )
#> Source: matrix 
#>   Nodes (8): Plan, Monitor, Adapt, Reflect, Discuss, Synthesize, Evaluate, Share
#>   Edges: 5 / 56 (density: 8.9%)
#>   Weights: [0.400, 0.490]  |  mean: 0.446
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
