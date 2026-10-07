# Coleman-Theil Hierarchy Index

The Coleman-Theil index (Burt 1992) measures how concentrated Burt's
dyadic constraint is across the \\d_i\\ contacts of node \\i\\. Let
\\p\_{ij}\\ be the share of the mutual tie strength of \\i\\ invested in
\\j\\, \\c\_{ij} = (p\_{ij} + \sum_q p\_{iq} p\_{qj})^2\\ the
constraint, and \\r\_{ij}\\ the constraint divided by its mean over the
contacts. Then \$\$H_i = \frac{\sum\_{j \in N(i)} r\_{ij} \log
r\_{ij}}{d_i \log d_i}.\$\$

## Usage

``` r
centrality_coleman_theil(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The mutual tie strength of a pair is the sum of the weights in both
directions, so direction is combined and loops are removed. Edge weights
must be finite and nonnegative, and `weighted = FALSE` gives every edge
weight one before the directions are combined. The index lies between
zero, for equal constraints, and one, for constraint concentrated on one
contact. Following Burt's STRUCTURE 4.2 manual (pages 181-183), an
isolated node scores zero and a node with one contact scores one. The
organizational and oligopoly multipliers of STRUCTURE are fixed at one.
A weight range beyond double precision raises an error.

## References

Burt, R. S. (1992). Structural Holes: The Social Structure of
Competition. Harvard University Press.
[doi:10.4159/9780674029095](https://doi.org/10.4159/9780674029095) .

## See also

[`centrality_constraint`](https://sonsoles.me/cograph/reference/centrality_constraint.md),
[`centrality_effective_size`](https://sonsoles.me/cograph/reference/centrality_effective_size.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_coleman_theil(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.07165311 0.10844104 0.15612256 0.06285124 0.17918268 0.05549900 0.39363179 
#>   Evaluate     Create      Share 
#> 0.12236245 0.14733940 0.05273089 
```
