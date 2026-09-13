# Dynamical importance by exact vertex deletion

Restrepo, Ott & Hunt's node dynamical importance is the relative drop in
adjacency spectral radius on removing that node: \\I_i =
(\rho(A)-\rho(A\_{-i}))/\rho(A)\\ (equation 2). This function recomputes
the spectral radius after every deletion. The paper's left/right
eigenvector product (equation 5) is an approximation and can differ
substantially on small networks; it is not used here.

## Usage

``` r
centrality_dynamical_importance(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The default `normalized = FALSE` preserves the published relative
  loss; `TRUE` additionally divides positive scores by their maximum.

## Value

Named numeric vector in input node order.

## Details

Supports directed or undirected nonnegative weighted networks.
Self-loops are always removed, as in the paper's zero-diagonal
definition. Edge weights, `weighted` and `simplify` follow the same
adjacency conventions as
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md).
The measure is invariant to reversing all arcs and ignores `mode` and
path-weight inversion. Disconnected graphs use the spectral radius of
the whole graph.

When the original spectral radius is zero (including any directed
acyclic graph), the ratio is undefined and all vertices receive `NaN`.
Isolates in a graph with positive spectral radius receive zero. The
empty graph returns an empty vector. Strong components are evaluated
separately so acyclic parts contribute exactly zero, avoiding numerical
eigenvalues of nilpotent blocks. Roundoff in the final ratio is clipped
to zero or one.

Repeated eigendecomposition is costly. Select this measure explicitly or
use `include = "dynamical_importance"`; it is held back from the default
`type = "all"` tier.

## References

Restrepo, J. G., Ott, E., & Hunt, B. R. (2006). Characterizing the
Dynamical Importance of Network Nodes and Links. Physical Review
Letters, 97, 094102.
[doi:10.1103/PhysRevLett.97.094102](https://doi.org/10.1103/PhysRevLett.97.094102)
.

## Examples

``` r
centrality_dynamical_importance(igraph::make_full_graph(4))
#>         1         2         3         4 
#> 0.3333333 0.3333333 0.3333333 0.3333333 
```
