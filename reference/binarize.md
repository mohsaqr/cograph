# Binarize Edge Weights

Replaces every surviving weight with 1 and drops edges whose weight is
at or below the threshold. The operation corresponds to
[`sna::event2dichot()`](https://rdrr.io/pkg/sna/man/event2dichot.html)
with an absolute threshold.

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

  Logical. If TRUE, negative edges become `-1`, which preserves the sign
  of the association. Default FALSE.

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
binarize(regulation_net, threshold = 0.1)
#> Cograph network: 10 nodes, 27 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 27 / 90 (density: 30.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     Adapt -> Explore  1.000
#>     Discuss -> Explore  1.000
#>     Create -> Explore  1.000
#>     Synthesize -> Plan  1.000
#>     Share -> Plan  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
