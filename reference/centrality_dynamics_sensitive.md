# Dynamics-sensitive centrality

Liu et al.'s finite-time dynamics-sensitive (DS) centrality is
\\S(T)=\sum\_{r=0}^{T-1}\beta A\[\beta A+(1-\mu)I\]^r\mathbf{1}\\, where
beta is the spreading rate and mu the recovery rate (equation 5 in the
preprint). This is the full recovery-parameter family. For mu=1, it
reduces to \\\sum\_{t=1}^{T}(\beta A)^t\mathbf{1}\\ (equation 7), also
the form listed in the Centrality Zoo. For mu=0 it gives the paper's
susceptible-infected case.

## Usage

``` r
centrality_dynamics_sensitive(x, ds_beta = 0.1, ds_mu = 1, ds_steps = 5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ds_beta:

  Finite spreading rate between 0 and 1, default 0.1.

- ds_mu:

  Finite recovery rate between 0 and 1, default 1.

- ds_steps:

  Nonnegative integer horizon, default 5. Must not exceed
  `.Machine$integer.max`.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive scores are divided by their
  maximum.

## Value

Named numeric vector in input node order.

## Details

Uses the simple undirected unweighted skeleton, as in the source: either
direction creates an edge, parallel edges count once and loops are
removed. The projection of other inputs is an explicit cograph
convention. `mode`, edge weights and shortest-path weight inversion do
not affect this measure. Isolates score zero. T=0 or beta=0 returns
zero; T=1 gives beta times degree. The initial seed itself is not added
to the score.

This linearized cumulative spreading score allows repeated walks and can
exceed the number of nodes. It is not a bounded infection probability or
an exact simulation of the nonlinear SIR/SI process. Defaults beta=0.1,
mu=1 and T=5 select a parameter setting studied in the paper; they are
not fitted to the input network. Any finite horizon is supported without
a spectral convergence condition, subject to numerical precision.
Overflow raises an error, even if normalization is requested.

## References

Liu, J. G., Lin, J. H., Guo, Q., & Zhou, T. (2016). Locating influential
nodes via dynamics-sensitive centrality. Scientific Reports, 6, 21380.
[doi:10.1038/srep21380](https://doi.org/10.1038/srep21380) . Preprint
equations 5 and 7: <https://arxiv.org/abs/1504.06672>.

## See also

[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md).

## Examples

``` r
g <- igraph::make_ring(5)
centrality_dynamics_sensitive(g, ds_beta = 0.1, ds_mu = 1, ds_steps = 5)
#>       1       2       3       4       5 
#> 0.24992 0.24992 0.24992 0.24992 0.24992 
centrality_dynamics_sensitive(g, ds_mu = 0)
#>       1       2       3       4       5 
#> 1.48832 1.48832 1.48832 1.48832 1.48832 
```
