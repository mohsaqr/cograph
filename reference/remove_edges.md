# Remove Edges from a Network

Remove Edges from a Network

## Usage

``` r
remove_edges(
  x,
  from,
  to,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- from:

  Source nodes, by label or index.

- to:

  Target nodes, by label or index. The same length as `from`.

- keep_isolates:

  Logical. Keep nodes that end up with no edges? Default TRUE, matching
  [`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md).

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` without those edges, or the input format when
`keep_format = TRUE`. Named pairs that carry no edge are reported in a
`cograph_no_such_edge` warning.

## See also

[`add_edges`](https://sonsoles.me/cograph/reference/add_edges.md),
[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

remove_edges(adj, from = "A", to = "B")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- C  1.000
#>     B -- C  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
