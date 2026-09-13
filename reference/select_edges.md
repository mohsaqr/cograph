# Select Edges with Lazy Computation

A powerful edge selection function with lazy computation (only computes
metrics actually referenced), multiple selection modes, and structural
awareness (bridges, communities, reciprocity).

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

  Logical. If TRUE, matrix, igraph, and statnet network inputs are
  returned in that format. Default FALSE returns cograph_network.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

- .keep_isolates:

  Deprecated. Use `keep_isolates`.

## Value

A cograph_network object with selected edges. If `keep_format = TRUE`,
matrix, igraph, and statnet network inputs are converted back to that
type.

## Details

Selection modes are combined with AND logic:

- `select_edges(x, top = 10, involving = "A")` selects top 10 edges
  **among those involving node A**

- All criteria must be satisfied for an edge to be selected

Edge metrics are computed lazily - only those actually referenced in
expressions or required by selection modes are computed.

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`select_bridges`](https://sonsoles.me/cograph/reference/select_bridges.md),
[`select_top_edges`](https://sonsoles.me/cograph/reference/select_top_edges.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
                .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

select_edges(adj, weight > 0.5)
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.600, 0.800]  |  mean: 0.700
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
select_edges(adj, top = 3)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
select_edges(adj, involving = "A")
#> Warning: 1 node(s) have no edges left. Nodes are kept; call remove_isolates() to drop them.
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.500, 0.800]  |  mean: 0.650
#>   Strongest edges:
#>     A -- C  0.800
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
select_edges(adj, between = list(c("A", "B"), c("C", "D")))
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.300, 0.800]  |  mean: 0.567
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     B -- C  0.300
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
