# Convert an Undirected Network to Directed

Convert an Undirected Network to Directed

## Usage

``` r
to_directed(
  x,
  mode = c("mutual", "arbitrary"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- mode:

  `"mutual"` (default) creates an arc in both directions for every
  undirected edge; `"arbitrary"` keeps one arc per edge, running from
  the lower node index to the higher.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. Directedness to read the input with.

## Value

A directed `cograph_network`, or the input format when
`keep_format = TRUE`.

## See also

[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md),
[`reverse_edges`](https://sonsoles.me/cograph/reference/reverse_edges.md)

## Examples

``` r
adj <- matrix(c(0, 1, 0,
                1, 0, 1,
                0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

to_directed(adj)
#> Cograph network: 3 nodes, 4 edges ( directed )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 4 / 6 (density: 66.7%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     B -> A  1.000
#>     A -> B  1.000
#>     C -> B  1.000
#>     B -> C  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
to_directed(adj, mode = "arbitrary")
#> Cograph network: 3 nodes, 2 edges ( directed )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -> B  1.000
#>     B -> C  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
