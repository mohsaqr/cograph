# Semi-Local Iterative Algorithm

The semi-local iterative algorithm (semi-IA; Luan et al. 2021) repeats
an update in the manner of eigenvector centrality and HITS, in which a
node collects the normalised scores of the nodes within \\r\\ steps,
weighted by the number of shortest paths to them and divided by the
distance: \$\$X_i(t + 1) = \sum\_{j \ne i:\\ d\_{ij} \le r}
\frac{n\_{ij}^{\gamma}}{d\_{ij}} \frac{X_j(t)}{\\X(t)\\}, \qquad X(0) =
1,\$\$ with \\d\_{ij}\\ the distance and \\n\_{ij}\\ the number of
shortest paths. The score is \\X\\ at the steady state.

## Usage

``` r
centrality_semi_ia(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `semi_ia_radius` (default `NULL`, 3 or 5 by the size
  of the network), `semi_ia_gamma` (default 0.2) and `semi_ia_max_iter`
  (default 10000).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The source prints its equation (8) with the node's own normalised score
\\X'\_i\\ inside the sum. Read so, the update ranks the nodes by the row
sums of the weight matrix and its normalised vector collapses onto one
node, which the source's Fig. 7 contradicts. The neighbor's score
\\X'\_j\\ is used, the reading of the eigenvector and HITS updates the
source says it follows. The norm is Euclidean, as in the source's HITS
normalisation. The steady state is the leading eigenvalue times the unit
leading eigenvector of the weight matrix. `semi_ia_radius = NULL`, the
default, takes \\r = 3\\ below 1000 nodes and \\r = 5\\ from 1000 nodes
on, the rule of the source; `Inf` admits the whole network.
`semi_ia_gamma` is 0.2, the value the source chooses, and must lie in
\\\[0, 1\]\\. The source stops when two successive normalised vectors
differ by less than \\10^{-5}\\; here the largest difference must fall
below \\10^{-12}\\, within at most `semi_ia_max_iter` updates (default
10000). Past that bound the last iterate is returned with a
`cograph_no_converge` warning; with `semi_ia_radius = 1` this happens on
a bipartite network, which has no steady state. The source treats
connected networks; on a disconnected network, or a one-node network,
the measure warns (class `cograph_undefined_measure`) and returns `NA`.
The source prints no score. It prints the monotonicity of the ranking on
the adjective-noun network (0.9997, Table 2), which the implementation
reproduces; the value does not separate the two readings of equation
(8), so the scores are otherwise checked against the definition only. On
four test networks the score ranks nodes closest to `icentr` (mean
Kendall correlation 0.90); with
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md)
the correlation is 0.69 to 0.84. `normalized = TRUE` divides the scores
by their maximum.

## References

Luan, Y., Bao, Z. and Zhang, H. (2021). Identifying influential
spreaders in complex networks by considering the impact of the number of
shortest paths. Journal of Systems Science and Complexity, 34(6),
2168-2181.
[doi:10.1007/s11424-021-0111-7](https://doi.org/10.1007/s11424-021-0111-7)
.

## See also

[`centrality_improved_closeness`](https://sonsoles.me/cograph/reference/centrality_improved_closeness.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_hub`](https://sonsoles.me/cograph/reference/centrality_authority.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_semi_ia(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.372646   2.511724   2.611233   2.534338   2.395257   2.410600   2.257798 
#>   Evaluate     Create      Share 
#>   2.419776   2.504134   2.394968 
centrality_semi_ia(to_undirected(regulation_net), semi_ia_gamma = 0.5)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.683296   2.836798   2.838076   2.939567   2.792207   2.802541   2.598788 
#>   Evaluate     Create      Share 
#>   2.801385   2.795802   2.719073 
```
