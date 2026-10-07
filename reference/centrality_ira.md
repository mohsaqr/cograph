# Iterative Resource Allocation

Iterative resource allocation (Ren et al. 2014) starts every node with
one unit of resource and repeatedly passes it to the neighbors in
proportion to the receiver's centrality \\\theta\\. The steady state
ranks the spreaders. \$\$I(t+1) = A\\I(t), \qquad a\_{ij} =
\frac{\theta_i^{\alpha}}{\sum\_{u \in \Gamma(j)} \theta_u^{\alpha}},
\qquad I(0) = (1, \dots, 1)\$\$

## Usage

``` r
centrality_ira(
  x,
  ira_mass = "coreness",
  ira_alpha = 1,
  ira_tol = 1e-06,
  ira_max_iter = 1000,
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ira_mass:

  Node centrality \\\theta\\: `"coreness"` (default) or `"degree"`.

- ira_alpha:

  Exponent \\\alpha\\ on the mass, a finite number (default 1).

- ira_tol:

  Positive stopping tolerance on the largest absolute change between
  iterates (default `1e-6`).

- ira_max_iter:

  Maximum number of iterations, a whole number of at least one (default
  1000).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The total resource is conserved, so on a graph without isolates the
scores sum to the number of nodes. An isolate receives nothing and
scores zero. The iteration stops when the largest absolute change falls
below `ira_tol`. On a bipartite component whose two vertex classes
differ in size, such as a star, the iteration has a period-two cycle. It
then reaches `ira_max_iter`, raises a `cograph_no_converge` warning and
returns the last iterate. The Centrality Zoo states the measure as the
left eigenvector of the transposed matrix, which agrees up to scale
where the limit exists. The implementation follows the source's
iteration.

## References

Ren, Z.-M., Zeng, A., Chen, D.-B., Liao, H. and Liu, J.-G. (2014).
Iterative resource allocation for ranking spreaders in complex networks.
EPL (Europhysics Letters), 106(4), 48005.
[doi:10.1209/0295-5075/106/48005](https://doi.org/10.1209/0295-5075/106/48005)
.

## See also

[`centrality_iira`](https://sonsoles.me/cograph/reference/centrality_iira.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ira(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.9259259  1.1111113  1.2962962  1.1111114  0.9259262  0.9259257  0.7407406 
#>   Evaluate     Create      Share 
#>  0.9259257  1.1111113  0.9259258 
```
