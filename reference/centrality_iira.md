# Improved Iterative Resource Allocation

Improved iterative resource allocation (Zhong, Liu and Shang 2015) is
[`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md)
with the receiver's share scaled by its spreading capacity \\1 - (1 -
\beta)^{k_i}\\, where \\k_i\\ is the degree and \\\beta\\ the spreading
rate. The recursion \\I(t+1) = A\\I(t)\\ runs a fixed number of steps
from \\I(0) = (1, \dots, 1)\\. \$\$a\_{ij} = \left\[1 - (1 -
\beta)^{k_i}\right\] \frac{\theta_i}{\sum\_{u \in \Gamma(j)}
\theta_u}\$\$

## Usage

``` r
centrality_iira(
  x,
  ira_mass = "coreness",
  iira_beta = 0.2,
  iira_steps = 50,
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ira_mass:

  Node centrality \\\theta\\: `"coreness"` (default) or `"degree"`.

- iira_beta:

  Spreading rate \\\beta\\, a number in \\(0, 1\]\\ (default 0.2).

- iira_steps:

  Number of iterations, a nonnegative whole number (default 50).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Every column of \\A\\ sums to less than one, so the scores decay
geometrically and only their order carries meaning. `normalized = TRUE`
divides them by their maximum. Each connected component decays at its
own rate, so raw scores are comparable only within a component, and a
large `iira_steps` underflows to zero. An isolate scores zero, and
`iira_steps = 0` returns a vector of ones. The Centrality Zoo (section
2.185) states a different matrix, which is stochastic in neither
direction and does not reproduce the source's worked example. The
implementation follows the source.

## References

Zhong, L.-F., Liu, J.-G. and Shang, M.-S. (2015). Iterative resource
allocation based on propagation feature of node for identifying the
influential nodes. Physics Letters A, 379(38), 2272-2276.
[doi:10.1016/j.physleta.2015.05.021](https://doi.org/10.1016/j.physleta.2015.05.021)
.

## See also

[`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_iira(regulation_net, normalized = TRUE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6118850  0.8072822  1.0000000  0.7927596  0.6000654  0.6172813  0.4448479 
#>   Evaluate     Create      Share 
#>  0.6368730  0.8172584  0.6391343 
```
