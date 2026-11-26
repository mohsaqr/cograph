# Mediative Effects Centrality

Mediative effects centrality (Friedkin 1991) measures how much a node
transmits the effects of the other nodes. The influence matrix \\W\\ is
the adjacency matrix with a unit diagonal, divided by its row sums. For
a node \\k\\, \\T\_{(k)} = (I - W\_{(k)})^{-1}\\, with \\W\_{(k)}\\ the
matrix \\W\\ without row and column \\k\\. Then \$\$\bar{t}\_{(k)j} =
\frac{\sum\_{i \ne j,k} t\_{(k)ij}}{(n-2)\\t\_{(k)jj}}, \qquad MEC_j =
\frac{\sum\_{k \ne j} \bar{t}\_{(k)j}}{n - 1}.\$\$

## Usage

``` r
centrality_mec(x, ...)
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
has fewer than three nodes.

## Details

The quotient \\t\_{(k)ij}/t\_{(k)jj}\\ is the probability that a
sequence of influence starting at \\i\\ passes through \\j\\ before it
reaches \\k\\. The score is the mean of that probability over the other
nodes \\i\\ and \\k\\, so it lies between 0 and 1. A node that every
sequence must pass through scores 1.

Direction is used, and weights, loops and parallel edges are ignored, so
the unit diagonal is calibrated against unit edges. `mode` has no
effect. The measure needs an irreducible chain, that is a connected
undirected or a strongly connected directed network, with at least three
nodes. Otherwise every score is `NA` with a `cograph_undefined_measure`
warning. The measure solves one linear system per node, so it is costly
and is computed only when requested by name or through `include`.

## References

Friedkin, N. E. (1991). Theoretical foundations for centrality measures.
American Journal of Sociology, 96(6), 1478-1504.
[doi:10.1086/229694](https://doi.org/10.1086/229694) .

## See also

[`centrality_iec`](https://sonsoles.me/cograph/reference/centrality_iec.md),
[`centrality_tec`](https://sonsoles.me/cograph/reference/centrality_tec.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mec(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6247310  0.3587844  0.7077012  0.6583109  0.5597441  0.3731031  0.3037538 
#>   Evaluate     Create      Share 
#>  0.2855696  0.5533676  0.5749343 
```
