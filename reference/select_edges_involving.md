# Select Edges Involving Nodes

Select edges where at least one endpoint is in the specified node set.

## Usage

``` r
select_edges_involving(
  x,
  nodes,
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- nodes:

  Character or integer. Node names or indices.

- ...:

  Additional filter expressions.

- keep_isolates:

  Keep nodes that end up with no edges? Default TRUE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with edges involving the specified nodes.

## See also

[`select_edges`](https://sonsoles.me/cograph/reference/select_edges.md),
[`select_edges_between`](https://sonsoles.me/cograph/reference/select_edges_between.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Edges involving A
select_edges_involving(adj, nodes = "A")
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

# Edges involving A or B
select_edges_involving(adj, nodes = c("A", "B"))
#> Cograph network: 4 nodes, 4 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 4 / 6 (density: 66.7%)
#>   Weights: [0.300, 0.800]  |  mean: 0.550
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     B -- C  0.300
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
