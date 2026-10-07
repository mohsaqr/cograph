# Dynamical Importance

Dynamical importance (Restrepo et al. 2006) is the relative drop in the
spectral radius \\\rho\\ of the adjacency matrix when the node is
removed: \$\$I_i = \frac{\rho(A) - \rho(A\_{-i})}{\rho(A)}.\$\$ The
spectral radius is recomputed after each deletion, in place of the
eigenvector approximation of the paper (eq. 5).

## Usage

``` r
centrality_dynamical_importance(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`) and `normalized` (default
  `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ holds the edge weights, or ones with `weighted = FALSE`, and
self-loops are removed. Negative or non-finite weights raise an error.
The scores lie between 0 and 1 and are unchanged when every arc is
reversed, so `mode` has no effect. A network with spectral radius 0,
such as any directed acyclic network, gives `NaN` for every node with a
`cograph_undefined_measure` warning. An isolated node in a network with
positive spectral radius scores 0.

## References

Restrepo, J. G., Ott, E., & Hunt, B. R. (2006). Characterizing the
Dynamical Importance of Network Nodes and Links. Physical Review
Letters, 97, 094102.
[doi:10.1103/PhysRevLett.97.094102](https://doi.org/10.1103/PhysRevLett.97.094102)
.

## See also

[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_resistance_curvature`](https://sonsoles.me/cograph/reference/centrality_resistance_curvature.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dynamical_importance(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.12106302 0.09396942 0.19308255 0.16302319 0.03448233 0.07603592 0.02203123 
#>   Evaluate     Create      Share 
#> 0.09419196 0.20502778 0.23434006 
```
