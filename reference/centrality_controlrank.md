# ControlRank centrality

Zhou, Yu and Lu's ControlRank is the smallest eigenvalue after deleting
a node's row and column from the symmetric part of the graph Laplacian.
With \\L = D-A\\, this is \\CR_i =
\lambda\_{\min}(((L+L^T)/2)\_{-i,-i})\\. D retains the original graph's
degrees: the Laplacian is not recomputed on the vertex-deleted graph.
Larger values receive higher rank.

## Usage

``` r
centrality_controlrank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides by the maximum if it is positive;
  otherwise raw scores are retained. This package normalization is
  optional and is not part of the published definition.

## Value

Named numeric vector in input node order.

## Details

Uses finite nonnegative interaction weights. For directed input,
\\A\_{ij}\\ denotes an arc from i to j and D contains outgoing
strengths. This fixes the row-Laplacian orientation explicitly;
transpose the input to use incoming strengths. Symmetrizing L preserves
its diagonal, so this is different from constructing a Laplacian of the
undirected projection. Directed scores can be negative and are not
clipped. For matrix inputs with very small weights, supply
`directed = TRUE` explicitly (or use a directed igraph object): the
shared input parser's approximate symmetry detection can otherwise infer
an undirected graph.

Loops are removed and zero weights are absent connections. Parallel
weights follow the generic simplify rule; remaining parallel edges sum.
With `weighted = FALSE`, each remaining edge contributes one. Mode,
weight inversion for shortest paths and cutoff are ignored.

Connected undirected graphs with at least two nodes have positive
scores. Disconnected undirected graphs score zero for every node because
at least one component remains ungrounded. Empty graphs return no
scores; singletons return zero as an explicit extension of the undefined
empty minor. The source excludes isolates; the matrix formula here also
applies to disconnected directed graphs, whose scores may remain
negative.

This implements the spectral index, not a controller simulation, a
finite-feedback convergence rate, or an optimization over controller
sets. In particular, no general directed stability guarantee is inferred
from these scores. The paper's multi-node selection problem is separate.

Dense eigensolves take O(n to the fourth) time and O(n squared) memory;
this measure is marked costly and excluded from the default all tier.
Disconnected blocks are solved separately, preserving isolated zeros
before normalization. Global scaling avoids intermediate overflow.
Unrepresentable weight ranges and unresolved positive spectra raise
errors. Signed directed scores near zero can retain floating-point
roundoff; very small raw scores can underflow. Uniform weight scaling
multiplies raw scores by the same factor.

## References

Zhou, J., Yu, X. and Lu, J.-A. (2019; online 2018). Node Importance in
Controlled Complex Networks. IEEE Transactions on Circuits and Systems
II: Express Briefs, 66(3), 437-441. Section III-C, Theorem 3; Figure 1
and section IV-A.
[doi:10.1109/TCSII.2018.2845940](https://doi.org/10.1109/TCSII.2018.2845940)
.

## Examples

``` r
centrality_controlrank(igraph::make_ring(5))
#>        1        2        3        4        5 
#> 0.381966 0.381966 0.381966 0.381966 0.381966 
centrality_controlrank(igraph::make_star(6, mode = "undirected"))
#>         1         2         3         4         5         6 
#> 1.0000000 0.1715729 0.1715729 0.1715729 0.1715729 0.1715729 
```
