# Invert Edge Weights (Similarity to Distance and Back)

Turns strong ties into short distances, which is what path-based
measures need when the weights are similarities rather than costs.

## Usage

``` r
invert_weights(
  x,
  method = c("reciprocal", "max_minus", "reflect"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- method:

  How to invert:

  `"reciprocal"`

  :   (default) `1 / w`. The standard similarity-to-distance map;
      requires non-zero weights, which every stored edge has.

  `"max_minus"`

  :   `max(w) - w`. The strongest edge becomes zero and is therefore
      dropped; a `cograph_edges_dropped` warning says how many.

  `"reflect"`

  :   `max(w) + min(w) - w`. Reverses the order of the weights while
      keeping every edge, so no edge is lost.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with inverted weights, or the input format when
`keep_format = TRUE`.

## See also

[`normalize_weights`](https://sonsoles.me/cograph/reference/normalize_weights.md),
[`shortest_paths`](https://sonsoles.me/cograph/reference/shortest_paths.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

invert_weights(adj)
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [1.250, 3.333]  |  mean: 2.150
#>   Strongest edges:
#>     B -- C  3.333
#>     C -- D  2.500
#>     A -- B  2.000
#>     B -- D  1.667
#>     A -- C  1.250
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
invert_weights(adj, method = "reflect")
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [0.300, 0.800]  |  mean: 0.580
#>   Strongest edges:
#>     B -- C  0.800
#>     C -- D  0.700
#>     A -- B  0.600
#>     B -- D  0.500
#>     A -- C  0.300
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
