# Add Edges to a Network

Add Edges to a Network

## Usage

``` r
add_edges(x, from, to, weight = 1, ..., keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- from:

  Source nodes, by label or index.

- to:

  Target nodes, by label or index. The same length as `from`.

- weight:

  Numeric weight for the new edges, length 1 or `length(from)`. Default
  1.

- ...:

  Named vectors of extra edge attributes, length 1 or `length(from)`.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the new edges, or the input format when
`keep_format = TRUE`. An edge that already exists has its weight
replaced, and a `cograph_edges_replaced` warning says how many.

## Note

When the igraph package is attached it masks this function with
[`igraph::add_edges()`](https://r.igraph.org/reference/add_edges.html),
which takes an igraph object. Use `cograph::add_edges()` to be explicit.

## See also

[`remove_edges`](https://sonsoles.me/cograph/reference/remove_edges.md),
[`add_nodes`](https://sonsoles.me/cograph/reference/add_nodes.md),
[`bind_networks`](https://sonsoles.me/cograph/reference/bind_networks.md)

## Examples

``` r
adj <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
adj["A", "B"] <- adj["B", "A"] <- 1

add_edges(adj, from = "B", to = "C", weight = 0.5)
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.500, 1.000]  |  mean: 0.750
#>   Strongest edges:
#>     A -- B  1.000
#>     B -- C  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
