# Combine Two Networks

Aligns two networks on node labels and combines their edges.

## Usage

``` r
bind_networks(
  x,
  y,
  method = c("union", "intersection", "difference"),
  weight = c("sum", "mean", "max", "min", "first"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x, y:

  Network inputs.

- method:

  How to combine the edge sets:

  `"union"`

  :   (default) every edge of either network, over the union of the node
      sets

  `"intersection"`

  :   only edges present in both, over the nodes common to both

  `"difference"`

  :   edges of `x` that are not in `y`, over the nodes of `x`

- weight:

  How to combine the weights of an edge present in both: `"sum"`
  (default), `"mean"`, `"max"`, `"min"`, or `"first"` (keep `x`'s
  weight).

- keep_format:

  Logical. Return `x`'s format when TRUE.

- directed:

  Logical or NULL. If NULL (default), the result is directed when either
  input is.

## Value

A `cograph_network` over the combined node set, or `x`'s format when
`keep_format = TRUE`. Nodes are ordered with `x`'s first, then any node
only `y` has.

## See also

[`add_edges`](https://sonsoles.me/cograph/reference/add_edges.md),
[`plot_difference`](https://sonsoles.me/cograph/reference/plot_difference.md)

## Examples

``` r
a <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
a["A", "B"] <- a["B", "A"] <- 1
b <- matrix(0, 3, 3, dimnames = list(c("B", "C", "D"), c("B", "C", "D")))
b["B", "C"] <- b["C", "B"] <- 2

bind_networks(a, b)
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [1.000, 2.000]  |  mean: 1.500
#>   Strongest edges:
#>     B -- C  2.000
#>     A -- B  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
bind_networks(a, b, method = "difference")
#> Cograph network: 3 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 1 / 3 (density: 33.3%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
