# Convert a Directed Network to Undirected

Collapses each pair of opposite arcs into one undirected edge. The
counterpart of
[`igraph::as_undirected()`](https://r.igraph.org/reference/as_directed.html)
and tidygraph's `to_undirected()`.

## Usage

``` r
to_undirected(
  x,
  method = c("max", "sum", "mean", "min", "mutual"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- method:

  How to combine `w[i, j]` and `w[j, i]`: `"max"` (default), `"sum"`,
  `"mean"`, `"min"`, or `"mutual"` (keep only reciprocated pairs, taking
  the minimum weight).

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. Directedness to read the input with.

## Value

An undirected `cograph_network`, or the input format when
`keep_format = TRUE`. Zero is how this representation stores "no edge",
so any pair whose combined weight is exactly zero disappears: every
unreciprocated arc under `method = "mutual"`, and a cancelling pair
under `"sum"`. A `cograph_edges_dropped` warning says how many.

## See also

[`to_directed`](https://sonsoles.me/cograph/reference/to_directed.md),
[`symmetrize`](https://sonsoles.me/cograph/reference/symmetrize.md)

## Examples

``` r
adj <- matrix(c(0, .5, 0,
                .2, 0, .7,
                0, 0, 0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

to_undirected(adj, method = "sum")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.700, 0.700]  |  mean: 0.700
#>   Strongest edges:
#>     A -- B  0.700
#>     B -- C  0.700
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
to_undirected(adj, method = "mutual")
#> Warning: 2 edge(s) combined to weight zero and were dropped; zero is how this representation stores 'no edge'.
#> Cograph network: 3 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 1 / 3 (density: 33.3%)
#>   Weights: [0.200, 0.200]  |  mean: 0.200
#>   Strongest edges:
#>     A -- B  0.200
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
