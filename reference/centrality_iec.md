# Immediate Effects Centrality

Immediate effects centrality (Friedkin 1991) is the reciprocal of the
mean length of the influence sequences that end at a node. The influence
matrix \\W\\ is the adjacency matrix with a unit diagonal, divided by
its row sums. With \\c\\ its stationary vector, \\Z = (I - W +
\mathbf{1}c')^{-1}\\ and the mean first passage times \\M = (I - Z + E
Z\_{dg})\\\mathrm{diag}(1/c)\\: \$\$IEC_j = \frac{n - 1}{\sum\_{i \ne j}
m\_{ij}}\$\$

## Usage

``` r
centrality_iec(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the influence chain is reducible or the network
has one node.

## Details

Direction is used, and weights, loops and parallel edges are ignored, so
the unit diagonal is calibrated against unit edges. `mode` has no
effect. The measure needs an irreducible chain, that is a connected
undirected or a strongly connected directed network. Otherwise, and on a
single node, every score is `NA` with a `cograph_undefined_measure`
warning. The measure differs from
[`centrality_markov`](https://sonsoles.me/cograph/reference/centrality_markov.md),
which omits the unit diagonal and divides by \\n\\, and the two can rank
nodes differently. The measure is costly and is computed only when
requested by name or through `include`.

## References

Friedkin, N. E. (1991). Theoretical foundations for centrality measures.
American Journal of Sociology, 96(6), 1478-1504.
[doi:10.1086/229694](https://doi.org/10.1086/229694) .

## See also

[`centrality_markov`](https://sonsoles.me/cograph/reference/centrality_markov.md),
[`centrality_random_walk`](https://sonsoles.me/cograph/reference/centrality_random_walk.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_iec(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.12812742 0.04122807 0.16167772 0.13181762 0.10049616 0.04665442 0.03461014 
#>   Evaluate     Create      Share 
#> 0.03095393 0.09086872 0.09704518 
```
