# Mapping Entropy Betweenness

Mapping entropy betweenness (MEB; Gialampoukidis et al. 2016) is mapping
entropy with the normalised betweenness in place of the degree
centrality: \$\$MEB_i = -BC_i \sum\_{j \in N(i),\\ BC_j \> 0} \log BC_j,
\qquad BC_i = \frac{2\\ b_i}{(N - 1)(N - 2)},\$\$ with \\b_i\\ the
betweenness of \\i\\ (the shortest-path share over unordered pairs of
other nodes) and \\N\\ the number of nodes.

## Usage

``` r
centrality_mapping_entropy_betweenness(x, ...)
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

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored. A
neighbor of betweenness 0 has \\\log 0\\; the source does not say what
to do, and such neighbors are left out of the sum (owner decision
2026-11-26). The normalisation is by the number of pairs of other nodes,
as the Centrality Zoo transcribes the source, so \\BC \le 1\\ and the
score is never negative. The logarithm is natural, as in
[`centrality_mapping_entropy`](https://sonsoles.me/cograph/reference/centrality_mapping_entropy.md);
another base rescales every score by one constant. A node of betweenness
0 scores 0, so does every node of a network with fewer than three nodes,
and a neighbor of betweenness 1 (the centre of a star) adds 0. The
source paper was read for an earlier source audit of this package but
was not available for this implementation; the formula is the one that
audit recorded and the one the Centrality Zoo prints. No score is
printed, so the implementation is checked against the definition only.
On four test networks the score ranks nodes closest to
[`centrality_delta_betweenness`](https://sonsoles.me/cograph/reference/centrality_length_scaled_betweenness.md)
(mean Kendall correlation 0.956),
[`centrality_relative_entropy`](https://sonsoles.me/cograph/reference/centrality_relative_entropy.md)
(0.949) and
[`centrality_proximal_betweenness`](https://sonsoles.me/cograph/reference/centrality_proximal_betweenness.md)
(0.948); with
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
the correlation is 0.92 to 0.96 and with
[`centrality_mapping_entropy`](https://sonsoles.me/cograph/reference/centrality_mapping_entropy.md)
0.71 to 0.87. `normalized = TRUE` divides the scores by their maximum.

## References

Gialampoukidis, I., Kalpakis, G., Tsikrika, T., Vrochidis, S. and
Kompatsiaris, I. (2016). Key player identification in terrorism-related
social media networks using centrality measures. In 2016 European
Intelligence and Security Informatics Conference (EISIC), 112-115.
[doi:10.1109/EISIC.2016.029](https://doi.org/10.1109/EISIC.2016.029) .

## See also

[`centrality_mapping_entropy`](https://sonsoles.me/cograph/reference/centrality_mapping_entropy.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mapping_entropy_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5853559  1.3269288  1.7952109  1.6733983  0.9918791  0.5644285  0.1896510 
#>   Evaluate     Create      Share 
#>  0.4105651  0.9992013  0.3789492 
```
