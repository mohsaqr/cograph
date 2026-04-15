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
  .keep_isolates = FALSE,
  keep_format = FALSE,
  directed = NULL
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
      `to_strength`, `edge_betweenness`, `is_bridge`, `is_mutual`,
      `same_community`

- top:

  Integer. Select top N edges by a metric.

- by:

  Character. Metric for top selection. Default `"weight"`. Options:
  `"weight"`, `"abs_weight"`, `"edge_betweenness"`.

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

- .keep_isolates:

  Logical. Keep nodes with no remaining edges? Default FALSE.

- keep_format:

  Logical. If TRUE, return the same format as input. Default FALSE
  returns cograph_network.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A cograph_network object with selected edges. If `keep_format = TRUE`,
returns the same type as input.

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
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Expression-based (lazy - only computes what's needed)
select_edges(adj, weight > 0.5)
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.600, 0.800]  |  mean: 0.700
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#> Layout: none 
select_edges(adj, abs_weight > 0.4)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 

# Top N edges by weight
select_edges(adj, top = 3)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
select_edges(adj, top = 3, by = "edge_betweenness")
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.300, 0.600]  |  mean: 0.467
#>   Strongest edges:
#>     B -- D  0.600
#>     A -- B  0.500
#>     B -- C  0.300
#> Layout: none 

# Edges involving specific nodes
select_edges(adj, involving = "A")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.500, 0.800]  |  mean: 0.650
#>   Strongest edges:
#>     A -- C  0.800
#>     A -- B  0.500
#> Layout: none 
select_edges(adj, involving = c("A", "B"))
#> Cograph network: 4 nodes, 4 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 4 / 6 (density: 66.7%)
#>   Weights: [0.300, 0.800]  |  mean: 0.550
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     B -- C  0.300
#> Layout: none 

# Edges between two node sets
select_edges(adj, between = list(c("A", "B"), c("C", "D")))
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.300, 0.800]  |  mean: 0.567
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     B -- C  0.300
#> Layout: none 

# Bridge edges only
select_edges(adj, bridges_only = TRUE)
#> Warning: No edges match the selection criteria.
#> Cograph network: 0 nodes, 0 edges ( undirected )
#> Source: filtered 
#>   Nodes (0): 
#> Layout: none 

# Combined: top 3 edges involving A
select_edges(adj, involving = "A", top = 3)
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.500, 0.800]  |  mean: 0.650
#>   Strongest edges:
#>     A -- C  0.800
#>     A -- B  0.500
#> Layout: none 

# Using endpoint degrees
select_edges(adj, from_degree >= 3 | to_degree >= 3)
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [0.300, 0.800]  |  mean: 0.520
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     C -- D  0.400
#>     B -- C  0.300
#> Layout: none 

# Within-community edges
select_edges(adj, same_community)
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.600, 0.800]  |  mean: 0.700
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#> Layout: none 
```
