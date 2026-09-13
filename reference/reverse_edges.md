# Reverse Edge Direction

Transposes the weight matrix, so every arc runs the other way. TNA users
reach for this to look at where transitions came from rather than where
they went.

## Usage

``` r
reverse_edges(x, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with every edge reversed, or the input format when
`keep_format = TRUE`. An undirected network is returned unchanged, with
a `cograph_no_effect` warning.

## See also

[`to_directed`](https://sonsoles.me/cograph/reference/to_directed.md),
[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md)

## Examples

``` r
adj <- matrix(c(0, .5, 0,
                0, 0, .7,
                0, 0, 0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

reverse_edges(adj)
#> Cograph network: 3 nodes, 2 edges ( directed )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.500, 0.700]  |  mean: 0.600
#>   Strongest edges:
#>     C -> B  0.700
#>     B -> A  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
