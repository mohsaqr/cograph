# Edge Betweenness Community Detection

The Girvan-Newman algorithm. It repeatedly removes the edge with the
highest edge betweenness and keeps the partition with the highest
modularity. On a weighted graph igraph warns that the membership is
selected by modularity.

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

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Negative weights are replaced by their absolute values.

- directed:

  Logical. Whether edge directions are used. Default `TRUE`. `NULL` uses
  the direction of the network.

- edge.betweenness:

  Logical. Whether igraph stores the edge betweenness values. Default
  `TRUE`.

- merges:

  Logical. Whether igraph stores the merge matrix. Default `TRUE`.

- bridges:

  Logical. Whether igraph stores the bridge edges. Default `TRUE`.

- modularity:

  Logical. Whether igraph stores the modularity scores. Default `TRUE`.

- membership:

  Logical. Whether igraph computes the membership vector. Default
  `TRUE`.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md). Its
  only other argument, `directed`, is already taken by this function, so
  any further argument raises an "unused argument" error.

## Value

A `cograph_communities` data frame with columns `node` and `community`.
See
[`communities`](https://sonsoles.me/cograph/reference/communities.md)
for its attributes.

## References

Girvan, M., & Newman, M.E.J. (2002). Community structure in social and
biological networks. *PNAS*, 99(12), 7821-7826.

## Examples

``` r
community_edge_betweenness(igraph::make_graph("Zachary"))
#> Community structure (edge_betweenness)
#>   Nodes: 34  | Communities: 5  | Modularity: 0.4013 
#>   Sizes: 10, 6, 5, 12, 1 
#> 
#>  node community
#>     1         1
#>     2         1
#>     3         2
#>     4         1
#>     5         3
#>     6         3
#>     7         3
#>     8         1
#>     9         4
#>    10         5
#>    11         3
#>    12         1
#>    13         1
#>    14         1
#>    15         4
#>    16         4
#>    17         3
#>    18         1
#>    19         4
#>    20         1
#>    21         4
#>    22         1
#>    23         4
#>    24         4
#>    25         2
#>    26         2
#>    27         4
#>    28         2
#>    29         2
#>    30         4
#>    31         4
#>    32         2
#>    33         4
#>    34         4
```
