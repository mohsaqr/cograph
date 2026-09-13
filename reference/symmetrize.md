# Symmetrize a Directed Network

Combines each pair of opposite arcs into one undirected edge. The result
is an undirected network, so measures that branch on directedness see
the change.

## Usage

``` r
symmetrize(
  x,
  method = c("max", "min", "mean", "sum", "mutual", "upper", "lower"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- method:

  How to combine `w[i, j]` and `w[j, i]`:

  `"max"`

  :   (default) the larger of the two; on a binary network this is sna's
      "weak" rule

  `"min"`

  :   the smaller of the two

  `"mean"`

  :   their average

  `"sum"`

  :   their total

  `"mutual"`

  :   keep only reciprocated pairs, taking the smaller weight; on a
      binary network this is sna's "strong" rule

  `"upper"`

  :   take the upper triangle and mirror it

  `"lower"`

  :   take the lower triangle and mirror it

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. Directedness to read the input with; the result is
  always undirected.

## Value

An undirected `cograph_network`, or the input format when
`keep_format = TRUE`. The weight matrix satisfies
[`isSymmetric()`](https://rdrr.io/r/base/isSymmetric.html).

## Details

`"max"`, `"min"`, `"mean"` and `"sum"` combine two values only where
both arcs exist; an unreciprocated edge keeps its own weight rather than
being compared against the zero that stands for the missing arc. That
distinction matters for signed networks, where comparing a negative
weight against a structural zero would delete the edge. Use `"mutual"`
when an edge should survive only if it was reciprocated.

## References

Butts, C. T. (2008). Social network analysis with sna. *Journal of
Statistical Software*, 24(6), 1–51.

## See also

[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md),
[`normalize_weights`](https://sonsoles.me/cograph/reference/normalize_weights.md)

## Examples

``` r
adj <- matrix(c(0, .5, 0,
                .2, 0, .7,
                0, .1, 0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

symmetrize(adj, method = "max")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.500, 0.700]  |  mean: 0.600
#>   Strongest edges:
#>     B -- C  0.700
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
symmetrize(adj, method = "mean")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.350, 0.400]  |  mean: 0.375
#>   Strongest edges:
#>     B -- C  0.400
#>     A -- B  0.350
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
symmetrize(adj, method = "mutual")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.100, 0.200]  |  mean: 0.150
#>   Strongest edges:
#>     A -- B  0.200
#>     B -- C  0.100
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
