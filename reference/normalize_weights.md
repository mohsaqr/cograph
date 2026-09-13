# Normalize Edge Weights

Rescales the weight matrix. Row normalisation is what turns a transition
count matrix into the transition probabilities that TNA models use.

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

  :   divide by the total of all weights

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

A row (or column, or the whole matrix) whose total is zero is left at
zero rather than producing `NaN`: there is nothing to distribute. Rows
with a zero total are reported in a `cograph_zero_norm` warning so that
the zeros are a stated result rather than a silent one.

`"minmax"` maps the weakest edge to `.Machine$double.eps` rather than to
exactly 0, because 0 is how this representation stores "no edge":
mapping to it would delete the weakest edge instead of rescaling it.

`"max"`, `"sum"` and `"minmax"` rescale each edge independently and
therefore keep any extra edge columns. `"row"` and `"column"` scale an
edge by a total that differs at its two endpoints, so they break
symmetry and return a directed network.

Row and column normalisation are meaningful on directed networks. On an
undirected network they still work but break symmetry, so the result is
returned as directed.

## See also

[`binarize`](https://sonsoles.me/cograph/reference/binarize.md),
[`invert_weights`](https://sonsoles.me/cograph/reference/invert_weights.md),
[`symmetrize`](https://sonsoles.me/cograph/reference/symmetrize.md)

## Examples

``` r
counts <- matrix(c(0, 3, 1,
                   2, 0, 4,
                   5, 1, 0), 3, 3, byrow = TRUE)
rownames(counts) <- colnames(counts) <- c("A", "B", "C")

normalize_weights(counts, method = "row")
#> Cograph network: 3 nodes, 6 edges ( directed )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 6 / 6 (density: 100.0%)
#>   Weights: [0.167, 0.833]  |  mean: 0.500
#>   Strongest edges:
#>     C -> A  0.833
#>     A -> B  0.750
#>     B -> C  0.667
#>     B -> A  0.333
#>     A -> C  0.250
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
normalize_weights(counts, method = "max")
#> Cograph network: 3 nodes, 6 edges ( directed )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 6 / 6 (density: 100.0%)
#>   Weights: [0.200, 1.000]  |  mean: 0.533
#>   Strongest edges:
#>     C -> A  1.000
#>     B -> C  0.800
#>     A -> B  0.600
#>     B -> A  0.400
#>     C -> B  0.200
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
