# Truss, mixed-degree decomposition and local social-capital measures

Five measures with explicit definitions and numerical reference checks.
All use the simple, unweighted, undirected skeleton: either direction
creates an edge, parallel edges count once and self-loops are removed.
This projection is a cograph input convention; no directed or weighted
generalisation of the published measures is claimed. All isolates score
0.

## Usage

``` r
centrality_truss(x, ...)

centrality_mdd(x, mdd_lambda = 0.7, ...)

centrality_bridging_coefficient(x, ...)

centrality_godfather(x, ...)

centrality_support(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive scores are divided by their
  maximum.

- mdd_lambda:

  Exhausted-degree weight between 0 and 1, default 0.7.

## Value

Named numeric vector in input node order.

## Details

- `truss`:

  Maximum truss number of an incident edge (Malliaros et al. 2016). A
  k-truss requires at least k-2 triangles per edge within the surviving
  subgraph, matching NetworkX. An edge outside any triangle has truss
  number 2; a complete graph on k vertices has node truss number k. Some
  sources instead label by the triangle threshold, producing values two
  smaller.

- `mdd`:

  Mixed-degree decomposition (Zeng & Zhang 2013): repeatedly peel by
  residual degree plus `mdd_lambda` times exhausted degree. Nodes
  falling below the current shell threshold join that shell before the
  threshold advances. Zero recovers the k-core number; one recovers
  degree. Intermediate thresholds are real-valued. Default 0.7, as in
  the paper's worked example.

- `bridging_coefficient`:

  Hwang et al.'s reciprocal-degree ratio: \\(1/d_i) / \sum\_{j \in N(i)}
  1/d_j\\. This is the coefficient itself, before multiplication by
  betweenness.

- `godfather`:

  Jackson's Godfather index: the number of unordered pairs of neighbours
  with no edge between them. Equals \\d_i(d_i-1)/2\\ minus the number of
  triangles containing i.

- `support`:

  Jackson's supported relationships: the number of neighbours sharing at
  least one common neighbour with i. An edge is counted once even if it
  belongs to multiple triangles.

LocalRank (Chen et al. 2012), also listed in the Centrality Zoo, is
already available as
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
on an undirected, unweighted graph; it needs no additional numerical
function.

## References

Malliaros, F. D., Rossi, M. E. G., & Vazirgiannis, M. (2016). Locating
influential nodes in complex networks. Scientific Reports, 6, 19307.
[doi:10.1038/srep19307](https://doi.org/10.1038/srep19307) .

Zeng, A., & Zhang, C. J. (2013). Ranking spreaders by decomposing
complex networks. Physics Letters A, 377, 1031-1035.
[doi:10.1016/j.physleta.2013.02.039](https://doi.org/10.1016/j.physleta.2013.02.039)
.

Hwang, W., Kim, T., Ramanathan, M., & Zhang, A. (2008). Bridging
centrality: graph mining from element level to group level. KDD '08,
336-344.
[doi:10.1145/1401890.1401934](https://doi.org/10.1145/1401890.1401934) .

Jackson, M. O. (2020). A typology of social capital and associated
network measures. Social Choice and Welfare, 54, 311-336.
[doi:10.1007/s00355-019-01189-3](https://doi.org/10.1007/s00355-019-01189-3)
.

Chen, D., Lu, L., Shang, M. S., Zhang, Y. C., & Zhou, T. (2012).
Identifying influential nodes in complex networks. Physica A, 391,
1777-1787.
[doi:10.1016/j.physa.2011.09.017](https://doi.org/10.1016/j.physa.2011.09.017)
.

## See also

[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md).

## Examples

``` r
adj <- matrix(1, 4, 4)
diag(adj) <- 0
centrality_truss(adj)
#> 1 2 3 4 
#> 4 4 4 4 
centrality_mdd(adj, mdd_lambda = 0.7)
#> 1 2 3 4 
#> 3 3 3 3 
centrality_support(adj)
#> 1 2 3 4 
#> 3 3 3 3 
```
