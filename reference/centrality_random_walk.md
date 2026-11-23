# Random Walk Centrality

Random walk centrality of Noh and Rieger (2004), the stationary
probability of a node over its characteristic relaxation time (their Eq.
10 and 11): \$\$C_i = \frac{P_i^\infty}{\tau_i}, \qquad \tau_i =
\sum\_{t = 0}^{\infty} \left(P\_{ii}(t) - P_i^\infty\right),\$\$ where
\\P\_{ii}(t)\\ is the probability that a walk started at \\i\\ is back
at \\i\\ after \\t\\ steps of a walk that moves to each out-neighbor
with equal probability, and \\P_i^\infty\\ its stationary probability
(\\K_i / \sum_j K_j\\ on an undirected network, Eq. 4).

## Usage

``` r
centrality_random_walk(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The relaxation time is computed as \\\tau_i = Z\_{ii} - P_i^\infty\\
from the fundamental matrix \\Z = (I - P + W)^{-1}\\ (Kemeny and Snell
1960), which is the series of Eq. 11 where it converges and its limit in
the mean on a bipartite network, where the return probabilities
oscillate. The reciprocal \\1 / C_j = \sum_i P_i^\infty m\_{ij}\\ is the
mean first passage time into \\j\\ from a start drawn from the
stationary distribution, so differences of the reciprocals give the
asymmetry of passage times, \\m\_{ij} - m\_{ji} = C_j^{-1} - C_i^{-1}\\
(Eq. 9).

Edge weights are ignored, as in the paper. The paper treats undirected
networks; on a strongly connected directed network the same Eq. 10 and
11 are evaluated with the stationary distribution of the directed walk.
On any other network every score is `NA`, with a
`cograph_undefined_measure` warning, because some passage times are
infinite.
[`tidygraph::centrality_random_walk()`](https://tidygraph.data-imaginist.com/reference/centrality.html)
and `netrankr`'s `"dist_rwalk"` with `"invsum"` report \\1 / \sum_i
m\_{ij}\\, the unweighted sum of passage times into the node, which is
not proportional to \\C_j\\.

Before cograph 2.7.21 the score was \\1 / \sum_j (m\_{ij} + m\_{ji}) /
2\\, which has no published source.

## References

Noh, J. D. and Rieger, H. (2004). Random walks on complex networks.
*Physical Review Letters*, 92(11), 118701.
[doi:10.1103/PhysRevLett.92.118701](https://doi.org/10.1103/PhysRevLett.92.118701)

Kemeny, J. G. and Snell, J. L. (1960). *Finite Markov Chains*. Van
Nostrand.

## See also

[`centrality_markov`](https://sonsoles.me/cograph/reference/centrality_markov.md),
[`centrality_current_flow_closeness`](https://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_random_walk(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.20932183 0.06088896 0.26259631 0.21855522 0.15238865 0.06853001 0.05111657 
#>   Evaluate     Create      Share 
#> 0.04409505 0.14373364 0.15987629 
```
