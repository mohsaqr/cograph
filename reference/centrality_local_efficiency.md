# Local efficiency, s-core, fragmentation, k-path census and EPC

Five node measures that other centrality packages expose and
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
did not. Each is a thin wrapper on
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Usage

``` r
centrality_local_efficiency(x, mode = "all", ...)

centrality_s_core(x, ...)

centrality_fragmentation(x, mode = "all", ...)

centrality_kpath(x, mode = "all", kpath_len = 3, ...)

centrality_epc(x, epc_threshold = 0.5, epc_runs = 1000, epc_seed = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- mode:

  Direction: `"all"`, `"out"` or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- kpath_len:

  Maximum path length for `centrality_kpath`. Default 3.

- epc_threshold:

  Edge removal probability. Default 0.5.

- epc_runs:

  Number of percolation realizations. Default 1000.

- epc_seed:

  Random seed. Default `NULL`, which leaves the caller's stream alone
  and makes the estimate vary between calls.

## Value

Named numeric vector, one value per node.

## Details

- `local_efficiency` (Latora & Marchiori 2001):

  The global efficiency of the subgraph induced on the node's neighbors,
  the node itself removed: the mean of \\1 / d\_{jl}\\ over ordered
  pairs of neighbors, with distances measured inside that subgraph.
  Nodes with fewer than two neighbors score 0. High values mark a node
  whose neighborhood survives its loss. Matches
  [`igraph::local_efficiency()`](https://r.igraph.org/reference/global_efficiency.html)
  and `brainGraph::efficiency(type = "local")`.

- `s_core` (Eidsaa & Almaas 2013):

  The weighted k-core: the largest strength threshold \\s\\ whose
  maximal subgraph of nodes with strength at least \\s\\ still contains
  the node. Unit weights give the k-core number exactly. Uses edge
  weights.

- `fragmentation` (Borgatti 2006):

  Distance-weighted fragmentation of the network after deleting the
  node: \\1 - \sum 1/d\_{ij} / ((n-1)(n-2))\\ over the ordered pairs
  that remain. Higher means a more disruptive removal. Matches
  [`keyplayer::fragment()`](https://rdrr.io/pkg/keyplayer/man/fragment.html)
  on unweighted input.

- `kpath` (Sade 1989):

  The number of simple paths of length at most `kpath_len` (default 3)
  that the node lies on, endpoints included; length 1 alone reproduces
  degree. Matches the per-vertex column sums of
  [`sna::kpath.census()`](https://rdrr.io/pkg/sna/man/path.census.html).
  Enumeration is exhaustive, so cost grows with branching factor to the
  power `kpath_len`.

- `epc` (Lin et al. 2008):

  Edge percolated component: each edge survives with probability
  `1 - epc_threshold`, and the score is the mean size of the node's
  component over `epc_runs` realizations, as a share of the network.
  cytoHubba and
  [`centiserve::epc()`](https://rdrr.io/pkg/centiserve/man/epc.html)
  divide by the node count alone, so their number is `epc_runs` times
  this one; the ranking is the same. A Monte Carlo estimate – pass
  `epc_seed` for a reproducible value.

`local_efficiency`, `fragmentation` and `kpath` follow `mode`; `s_core`
and `epc` read the undirected skeleton.

## References

Latora, V., & Marchiori, M. (2001). Efficient behavior of small-world
networks. Physical Review Letters, 87(19), 198701.

Eidsaa, M., & Almaas, E. (2013). s-core network decomposition: A
generalization of k-core analysis to weighted networks. Physical Review
E, 88(6), 062819.

Borgatti, S. P. (2006). Identifying sets of key players in a social
network. Computational and Mathematical Organization Theory, 12(1),
21-34.

Sade, D. S. (1989). Sociometrics of Macaca mulatta III: n-path
centrality in grooming networks. Social Networks, 11(3), 273-292.

Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
(2008). Hubba: hub objects analyzer. Nucleic Acids Research, 36,
W438-W443.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_weighted_kshell`](https://sonsoles.me/cograph/reference/centrality_weighted_kshell.md),
[`centrality_geodesic_kpath`](https://sonsoles.me/cograph/reference/centrality_weighted_kshell.md),
[`network_local_efficiency`](https://sonsoles.me/cograph/reference/network_local_efficiency.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_local_efficiency(adj)
#>         A         B         C         D         E         F 
#> 1.0000000 1.0000000 0.3333333 0.3333333 1.0000000 1.0000000 
centrality_s_core(adj)
#> A B C D E F 
#> 2 2 2 2 2 2 
centrality_fragmentation(adj)
#>         A         B         C         D         E         F 
#> 0.2833333 0.2833333 0.6000000 0.6000000 0.2833333 0.2833333 
centrality_kpath(adj, kpath_len = 2)
#>  A  B  C  D  E  F 
#>  6  6 10 10  6  6 
centrality_epc(adj, epc_runs = 50, epc_seed = 1)
#>         A         B         C         D         E         F 
#> 0.4733333 0.4866667 0.5400000 0.5033333 0.4766667 0.4066667 
```
