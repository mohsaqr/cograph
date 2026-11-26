# Global Importance of Nodes

The global importance of nodes (GIN; Zhao et al. 2020) multiplies a
factor for the node's own degree by the degrees of all other nodes, each
divided by its distance from the node: \$\$GIN(i) = e^{\alpha k_i / n}
\sum\_{j \ne i} \frac{k_j}{d\_{ij}},\$\$ with \\k\\ the degree,
\\d\_{ij}\\ the shortest-path distance and \\n\\ the number of nodes.

## Usage

``` r
centrality_gin(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `gin_alpha` (the weight of the node's own degree,
  default 1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`gin_alpha` is 1 by default, the value of the source. The source also
carries a factor \\\beta\\ in front of the sum, which rescales every
score alike and is fixed at 1 here. A node of degree zero scores 0. A
node that cannot be reached adds nothing to the sum, which the sources
consulted do not state. The equation was taken from two later papers
that reproduce it; the original article was not consulted.
`normalized = TRUE` divides the scores by their maximum.

## References

Zhao, J., Wang, Y. and Deng, Y. (2020). Identifying influential nodes in
complex networks from global perspective. Chaos, Solitons and Fractals,
133, 109637.
[doi:10.1016/j.chaos.2020.109637](https://doi.org/10.1016/j.chaos.2020.109637)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_gin(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   62.65141   72.88475   84.57761   71.97369   61.82705   63.47577   55.19751 
#>   Evaluate     Create      Share 
#>   65.12449   73.79581   65.12449 
```
