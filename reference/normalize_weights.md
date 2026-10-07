# Normalize Edge Weights

Rescales the edge weights. Row normalization turns a transition count
matrix into the transition probabilities used by TNA models.

## Usage

``` r
normalize_weights(
  x,
  method = c("row", "column", "max", "sum", "minmax"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- method:

  How to rescale:

  `"row"`

  :   (default) each row sums to 1

  `"column"`

  :   each column sums to 1

  `"max"`

  :   divide by the largest absolute weight

  `"sum"`

  :   divide by the sum of the weight matrix, in which each undirected
      edge appears twice

  `"minmax"`

  :   rescale the non-zero weights to \[0, 1\]

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with rescaled weights, or the input format when
`keep_format = TRUE`.

## Details

A row or column whose total is zero is left at zero. Zero totals, and a
zero denominator for `"max"` or `"sum"`, raise a `cograph_zero_norm`
warning.

`"minmax"` maps the weakest edge to `.Machine$double.eps`. A weight of
exactly 0 would remove the edge, because 0 stores "no edge". When all
weights are equal they all become 1.

`"max"`, `"sum"` and `"minmax"` rescale each edge independently and keep
any extra edge columns. `"row"` and `"column"` scale an edge by a total
that differs at its two endpoints. They break symmetry, so an undirected
input is returned as a directed network.

## See also

[`binarize`](https://sonsoles.me/cograph/reference/binarize.md),
[`invert_weights`](https://sonsoles.me/cograph/reference/invert_weights.md),
[`symmetrize`](https://sonsoles.me/cograph/reference/symmetrize.md)

## Examples

``` r
normalize_weights(regulation_net, method = "row")
#> Cograph network: 10 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 30 / 90 (density: 33.3%)
#>   Weights: [0.082, 0.750]  |  mean: 0.333
#>   Strongest edges:
#>     Reflect -> Monitor  0.750
#>     Synthesize -> Reflect  0.700
#>     Monitor -> Create  0.698
#>     Explore -> Reflect  0.565
#>     Evaluate -> Adapt  0.518
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
