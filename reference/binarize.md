# Binarize Edge Weights

Replaces every surviving weight with 1, dropping edges at or below the
threshold. The network equivalent of
[`sna::event2dichot()`](https://rdrr.io/pkg/sna/man/event2dichot.html).

## Usage

``` r
binarize(
  x,
  threshold = 0,
  absolute = TRUE,
  signed = FALSE,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- threshold:

  Numeric. Edges whose weight exceeds this value are kept and set to 1.
  Default 0, which keeps every existing edge.

- absolute:

  Logical. Compare `abs(weight)`. Default TRUE, so a correlation network
  keeps its strong negative edges.

- signed:

  Logical. If TRUE, negative edges become `-1` rather than `1`,
  preserving the sign of the association. Default FALSE.

- keep_isolates:

  Logical. Keep nodes that end up with no edges? Default TRUE.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` whose weights are all `1` (or, when `signed = TRUE`,
`1` for a positive edge and `-1` for a negative one), or the input
format when `keep_format = TRUE`. Nodes left without edges are kept and
reported in a `cograph_isolates_created` warning, unless
`keep_isolates = FALSE`.

## References

Butts, C. T. (2008). Social network analysis with sna. *Journal of
Statistical Software*, 24(6), 1–51.

## See also

[`threshold_edges`](https://sonsoles.me/cograph/reference/threshold_edges.md),
[`normalize_weights`](https://sonsoles.me/cograph/reference/normalize_weights.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

binarize(adj, threshold = 0.45)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#>     A -- C  1.000
#>     B -- D  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
