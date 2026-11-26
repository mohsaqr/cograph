# Entropy-Based Gravity Model

The entropy-based gravity model (Yan et al. 2020) combines the node
H-index, closeness, betweenness and PageRank of a node into one mass
with entropy weights, and adds the gravity between the node and the
nodes within \\r\\ steps: \$\$G(i) = \sum\_{j:\\ 1 \le d\_{ij} \le r}
\frac{C_i C_j}{d\_{ij}^2}, \qquad C_i = \sum\_{k=1}^{4} w_k b\_{ik},
\qquad w_k = \frac{1 - S_k}{4 - \sum_l S_l},\$\$ with \\d\_{ij}\\ the
distance, \\b\_{ik}\\ the \\k\\-th attribute of node \\i\\ min-max
scaled over the nodes, and \\S_k\\ the entropy of the shares \\b\_{ik} /
\sum_i b\_{ik}\\ divided by \\\ln N\\.

## Usage

``` r
centrality_entropy_gravity(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `entropy_gravity_radius` (default 3) and `damping`
  (the PageRank damping factor, default 0.85).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The source prints its equation (3) with the shares taken over the
attributes of one node and its equation (5) with the entropies \\S_k\\
in place of the weights \\w_k\\. Its text describes the entropy of each
attribute across the nodes, a lower entropy giving a larger weight, and
a combination by weighted averaging, which is what is computed (owner
decision 2026-11-26); read literally, equation (3) gives a negative
weight on the kite network. As in the source's equation (3), an
attribute at which only one node is above the minimum takes the shares
\\(1 + b\_{ik}) / \sum_i (1 + b\_{ik})\\ instead. The attributes are
those of the source's chosen combination: the node H-index
([`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md)),
closeness as the reciprocal of the sum of distances, betweenness, and
PageRank
([`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md))
with `damping`, which at `damping = 1` is the degree share, the
stationary distribution of the random walk; min-max scaling removes
their scale. `entropy_gravity_radius` is \\r\\, 3 by default as in the
source (the Centrality Zoo says 2); `Inf` admits every node. Scaled
values within \\10^{-9}\\ of 0 count as the minimum, so that nodes equal
in exact arithmetic are not separated by rounding. An attribute with the
same value at every node, as on any regular network, makes the min-max
scaling 0/0; the measure then warns (class `cograph_undefined_measure`)
and returns `NA`. It does the same on a disconnected or a one-node
network, where closeness has no value. The source prints no value, so
the implementation is checked against the definition only. On four test
networks the score ranks nodes closest to `multi_evidence` and `hybrid`
(mean Kendall correlation 0.90); with closeness the correlation is 0.77
to 0.92. A `entropy_gravity_radius` that is not a whole number of at
least one or `Inf` raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Yan, X.-L., Cui, Y.-P. and Ni, S.-J. (2020). Identifying influential
spreaders in complex networks based on entropy weight method and gravity
law. Chinese Physics B, 29(4), 048902.
[doi:10.1088/1674-1056/ab77fe](https://doi.org/10.1088/1674-1056/ab77fe)
.

## See also

[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality_egm`](https://sonsoles.me/cograph/reference/centrality_egm.md),
[`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md),
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_entropy_gravity(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.597388   2.619912   3.699044   2.729238   1.029741   1.728506   0.000000 
#>   Evaluate     Create      Share 
#>   1.741642   2.692761   1.730051 
centrality_entropy_gravity(to_undirected(regulation_net), entropy_gravity_radius = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.597388   2.619912   3.699044   2.729238   1.029741   1.728506   0.000000 
#>   Evaluate     Create      Share 
#>   1.741642   2.692761   1.730051 
```
