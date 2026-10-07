# Symmetrize a Directed Network

Combines each pair of opposite arcs into one undirected edge. The result
is an undirected network. Self-loops are kept unchanged.

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
[`isSymmetric()`](https://rdrr.io/r/base/isSymmetric.html). Zero is how
this representation stores "no edge", so any pair whose combined weight
is exactly zero disappears: every unreciprocated arc under
`method = "mutual"`, and a cancelling pair under `"sum"`. A
`cograph_edges_dropped` warning says how many.

## Details

`"max"`, `"min"`, `"mean"` and `"sum"` combine two values only where
both arcs exist. An unreciprocated edge keeps its own weight. In a
signed network an unreciprocated negative edge is therefore kept.
`"mutual"` keeps only reciprocated edges.

## References

Butts, C. T. (2008). Social network analysis with sna. *Journal of
Statistical Software*, 24(6), 1–51.

## See also

[`to_undirected`](https://sonsoles.me/cograph/reference/to_undirected.md),
[`normalize_weights`](https://sonsoles.me/cograph/reference/normalize_weights.md)

## Examples

``` r
symmetrize(regulation_net, method = "mean")
#> Cograph network: 10 nodes, 27 edges ( undirected )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 27 / 45 (density: 60.0%)
#>   Weights: [0.070, 0.490]  |  mean: 0.267
#>   Strongest edges:
#>     Plan -- Evaluate  0.490
#>     Monitor -- Share  0.490
#>     Adapt -- Evaluate  0.430
#>     Reflect -- Synthesize  0.420
#>     Plan -- Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
