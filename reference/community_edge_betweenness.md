# Edge Betweenness Community Detection

Girvan-Newman algorithm. Iteratively removes edges with highest
betweenness centrality to reveal community structure.

## Usage

``` r
community_edge_betweenness(
  x,
  weights = NULL,
  directed = TRUE,
  edge.betweenness = TRUE,
  merges = TRUE,
  bridges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)

com_eb(
  x,
  weights = NULL,
  directed = TRUE,
  edge.betweenness = TRUE,
  merges = TRUE,
  bridges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  ...
)
```

## Arguments

- x:

  Network input

- weights:

  Edge weights. NULL uses network weights, NA for unweighted.

- directed:

  Logical; treat graph as directed? Default TRUE.

- edge.betweenness:

  Logical; return edge betweenness values? Default TRUE.

- merges:

  Logical; return merge matrix? Default TRUE.

- bridges:

  Logical; return bridge edges? Default TRUE.

- modularity:

  Logical; return modularity scores? Default TRUE.

- membership:

  Logical; return membership vector? Default TRUE.

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](http://sonsoles.me/cograph/reference/detect_communities.md).

## References

Girvan, M., & Newman, M.E.J. (2002). Community structure in social and
biological networks. *PNAS*, 99(12), 7821-7826.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_edge_betweenness(g)
igraph::membership(comm)
#>  [1] 1 1 2 1 3 3 3 1 4 5 3 1 1 1 4 4 3 1 4 1 4 1 4 4 2 2 4 2 2 4 4 2 4 4
net <- as_cograph(matrix(runif(25), 5, 5))
com_eb(net)
#> Warning: At vendor/cigraph/src/community/edge_betweenness.c:503 : Membership vector will be selected based on the highest modularity score.
#> Community structure (edge_betweenness)
#>   Number of communities: 2 
#>   Modularity: 0.0167 
#>   Community sizes: 1, 4 
#>   Nodes: 5 
```
