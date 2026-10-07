# Node and Neighbor Layer Information Centrality

NINL (Zhu and Wang 2021) starts each node with the sum of the degrees
\\k_j\\ in its closed neighborhood of radius \\r\\ and then, for \\p\\
iterations, replaces every score by the sum of the previous scores of
its neighbors: \$\$NINL^{(p)} = A^p \\ NINL^{(0)}, \qquad NINL^{(0)}\_i
= \sum\_{j : d(i,j) \le r} k_j.\$\$

## Usage

``` r
centrality_ninl(x, ninl_order = 3, ninl_radius = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ninl_order:

  Number of iterations \\p\\, a nonnegative integer. Default 3, as in
  the paper.

- ninl_radius:

  Radius \\r\\. `NULL` (default) uses the automatic radius of the paper,
  a nonnegative integer fixes the hop radius, and `Inf` includes every
  reachable node.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `normalized` (divide by the maximum, default
  `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The default radius is the ceiling of the average shortest-path length,
as in the paper. On a disconnected network that average is infinite, so
the default radius covers the whole component of each node. Isolated
nodes score zero, and `ninl_order = 0` returns the initial degree sums.
With `normalized = TRUE` the scores are rescaled at every iteration, and
on a bipartite network they can alternate between iterations. An invalid
`ninl_order` or `ninl_radius`, and raw scores that overflow, raise an
error.

## References

Zhu, J. and Wang, L. (2021). Identifying Influential Nodes in Complex
Networks Based on Node Itself and Neighbor Layer Information. Symmetry,
13, 1570. [doi:10.3390/sym13091570](https://doi.org/10.3390/sym13091570)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ninl(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       7992       9828      11124       9504       7884       8046       6804 
#>   Evaluate     Create      Share 
#>       8586       9936       8640 
```
