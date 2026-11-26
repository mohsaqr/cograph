# Multi-Evidence Centrality

Multi-evidence centrality (MeC; Mo and Deng 2019) fuses four measures of
a node with Dempster's rule of combination. For each measure \\X\\, a
node is given a degree of belief that it is important and one that it is
not, \$\$m_X(h) = \frac{X_i - X\_{min}}{X\_{max} - X\_{min} + \omega},
\qquad m_X(l) = \frac{X\_{max} - X_i}{X\_{max} - X\_{min} + \omega},\$\$
and the rest is left undecided. The four assignments are combined, and
the score is the combined belief in "important" minus that in "not
important". The measures are degree, betweenness, efficiency and
correlation centrality.

## Usage

``` r
centrality_multi_evidence(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `evidence_omega` (default 1) and `coc_alpha` (the
  distance exponent of correlation centrality, default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Degree is divided by \\n - 1\\, betweenness by the number of pairs of
other nodes, and efficiency is the sum of reciprocal distances divided
by \\n\\; the fourth measure is
[`centrality_correlation_centrality`](https://sonsoles.me/cograph/reference/centrality_correlation_centrality.md)
with its `coc_alpha`. The score lies between -1 and 1. `evidence_omega`
is 1 by default, the value of the source's example; a smaller value
leaves less belief undecided and can change the ranking, although the
source says it does not. Nodes that cannot be reached add nothing to
efficiency. The implementation reproduces the 21 values the source
prints for the ARPA network to three decimals, on an edge list recovered
from the other columns of the same table. An `evidence_omega` that is
not above 0 and at most 1 raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Mo, H. and Deng, Y. (2019). Identifying node importance based on
evidence theory in complex networks. Physica A: Statistical Mechanics
and its Applications, 529, 121538.
[doi:10.1016/j.physa.2019.121538](https://doi.org/10.1016/j.physa.2019.121538)
.

## See also

[`centrality_correlation_centrality`](https://sonsoles.me/cograph/reference/centrality_correlation_centrality.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_multi_evidence(regulation_net)
#>     Explore        Plan     Monitor       Adapt     Reflect     Discuss 
#> -0.16583870  0.19407151  0.46352156  0.25217195 -0.09987659 -0.12320936 
#>  Synthesize    Evaluate      Create       Share 
#> -0.46590169 -0.13077413  0.16640845 -0.15980283 
```
