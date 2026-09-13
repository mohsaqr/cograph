# Complement of a Network

Every pair of distinct nodes that is not joined in `x` is joined in the
complement, and vice versa.

## Usage

``` r
complement_network(
  x,
  weight = 1,
  loops = FALSE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- weight:

  Numeric. Weight to give the new edges. Default 1. Zero is how this
  representation stores "no edge", so `weight = 0` raises a
  `cograph_bad_selection` error rather than returning an empty network.

- loops:

  Logical. Include self-loops in the complement. Default FALSE.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` holding the complement, or the input format when
`keep_format = TRUE`. Directedness is preserved.

## See also

[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md),
[`binarize`](https://sonsoles.me/cograph/reference/binarize.md)

## Examples

``` r
adj <- matrix(c(0, 1, 0,
                1, 0, 0,
                0, 0, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

complement_network(adj)
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
