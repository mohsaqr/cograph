# Dynamics-Sensitive Centrality

Dynamics-sensitive centrality (Liu et al. 2016) is a linearized
cumulative spreading score with spreading rate \\\beta\\ and recovery
rate \\\mu\\ over \\T\\ steps: \$\$S(T) = \sum\_{r=0}^{T-1} \beta A
\left\[\beta A + (1-\mu) I\right\]^r \mathbf{1}.\$\$ With \\\mu = 1\\ it
reduces to \\\sum\_{t=1}^{T} (\beta A)^t \mathbf{1}\\, the form listed
in the Centrality Zoo, and \\\mu = 0\\ gives the susceptible-infected
case.

## Usage

``` r
centrality_dynamics_sensitive(x, ds_beta = 0.1, ds_mu = 1, ds_steps = 5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ds_beta:

  Spreading rate \\\beta\\, between 0 and 1. Default 0.1.

- ds_mu:

  Recovery rate \\\mu\\, between 0 and 1. Default 1.

- ds_steps:

  Horizon \\T\\, a nonnegative integer. Default 5.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored. Isolated nodes score 0. \\T = 0\\
or \\\beta = 0\\ gives 0, and \\T = 1\\ gives \\\beta\\ times the
degree. The score counts repeated walks and can exceed the number of
nodes, so it is not an infection probability. The defaults \\\beta =
0.1\\, \\\mu = 1\\ and \\T = 5\\ are one setting studied by Liu et al.
(2016). Parameters outside their ranges raise an error.

## References

Liu, J. G., Lin, J. H., Guo, Q., & Zhou, T. (2016). Locating influential
nodes via dynamics-sensitive centrality. Scientific Reports, 6, 21380.
[doi:10.1038/srep21380](https://doi.org/10.1038/srep21380) .

## See also

[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dynamics_sensitive(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>    1.04422    1.25513    1.45154    1.23364    1.02868    1.05799    0.87553 
#>   Evaluate     Create      Share 
#>    1.09697    1.26998    1.09844 
```
