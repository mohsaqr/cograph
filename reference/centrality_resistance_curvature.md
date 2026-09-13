# Node resistance curvature

Devriendt and Lambiotte's node resistance curvature is
\\p_i=1-\frac{1}{2}\sum\_{j\sim i}w\_{ij}R\_{ij}\\, where weights are
electrical conductances and R is effective resistance. Equivalently, it
is one minus half the expected degree in a random spanning tree whose
probability is proportional to the product of its edge conductances. The
expectation is taken separately within each connected component.

## Usage

``` r
centrality_resistance_curvature(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, scores are divided by their positive
  maximum; negative values remain negative and the component-sum
  identity no longer holds. Default raw scores retain the published
  interpretation.

## Value

Named numeric vector in input node order.

## Details

Low, possibly negative, curvature characterizes tree-like junctions;
larger curvature characterizes locally redundant connections. It is a
geometric descriptor, not a universal ranking of influence. On a tree
the score is one minus half the degree; on an unweighted clique or cycle
of n vertices every node scores 1/n. Isolates score one, following the
empty sum. Raw scores sum to the number of connected components.

Uses finite nonnegative edge weights as conductances when
`weighted = TRUE`; zero weights are absent connections. Without weights,
uses the simple undirected skeleton. Self-loops are always removed. For
weighted directed inputs, opposite arcs are added to form undirected
conductances. The `simplify` argument combines parallel edges first;
remaining weighted parallel edges are added. These input projections are
cograph conventions for the source's undirected domain. `mode` and
shortest-path weight inversion do not affect the result.

Exact dense electrical systems are solved component by component, using
Cholesky factors of grounded Laplacians. Squared triangular-solve norms
avoid subtracting nearly equal pseudoinverse entries. Uniform rescaling
of conductances within a component leaves curvature unchanged. Extreme
weight ranges can still produce numerical singularity or overflow, in
which case an error is raised. Dense factorization and edge solves cost
up to O(n cubed + n squared times m) per component; the measure is
excluded from the default all tier and must be requested explicitly.

## References

Devriendt, K., & Lambiotte, R. (2022). Discrete curvature on graphs from
the effective resistance. Journal of Physics: Complexity, 3, 025008.
Definition 1, equation 2; Property 2; Appendix A.1, Theorem 2.
[doi:10.1088/2632-072X/ac730d](https://doi.org/10.1088/2632-072X/ac730d)
. <https://arxiv.org/abs/2201.06385>.

## Examples

``` r
centrality_resistance_curvature(igraph::make_star(5, mode = "undirected"))
#>    1    2    3    4    5 
#> -1.0  0.5  0.5  0.5  0.5 
```
