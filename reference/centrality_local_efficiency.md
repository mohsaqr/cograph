# Local Efficiency, s-Core, Fragmentation, k-Path and EPC

Local efficiency (Latora and Marchiori 2001) is the mean of
\\1/d\_{jl}\\ over ordered pairs of a node's neighbors, with distances
measured inside the subgraph induced on those neighbors. The s-core
index (Eidsaa and Almaas 2013) is the largest strength threshold \\s\\
whose s-core contains the node. Fragmentation (Borgatti 2006) is the
distance-weighted fragmentation of the network after the node is
deleted, \$\$F\_{-v} = 1 - \frac{\sum\_{i \ne j}
1/d\_{ij}}{(n-1)(n-2)}.\$\$ The k-path count (Sade 1989) is the number
of simple paths of length at most `kpath_len` that pass through or end
at the node. The edge percolated component (EPC; Lin et al. 2008) is the
mean size of the node's component, as a share of all nodes, when each
edge is kept with probability `1 - epc_threshold`.

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

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  Local efficiency and fragmentation use `weighted` (default `TRUE`),
  `invert_weights` (default `NULL`, which inverts for tna input only)
  and `alpha` (inversion exponent, default 1). The s-core index uses
  `weighted`.

- kpath_len:

  Maximum path length for the k-path count. Default 3. Length 1 gives
  the degree in the undirected skeleton.

- epc_threshold:

  Probability that an edge is removed in one realization. Default 0.5.

- epc_runs:

  Number of percolation realizations. Default 1000.

- epc_seed:

  Random seed. The default `NULL` uses the caller's random stream, so
  the estimate varies between calls. A seed gives a reproducible value
  and leaves the caller's stream unchanged.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Local efficiency, fragmentation and the k-path count follow `mode`, and
`mode = "all"` treats edges as undirected. Local efficiency and
fragmentation read edge weights as distances, so with weights below one
local efficiency exceeds one and fragmentation is negative;
`invert_weights = TRUE` converts weights to distances \\1/w^\alpha\\. On
unweighted input both lie between 0 and 1, and a node with fewer than
two neighbors has local efficiency 0. Fragmentation is `NaN` on a
network with fewer than three nodes. The s-core index symmetrizes the
network by the stronger of the two directions and equals the k-core
number when all weights are one. The k-path count and EPC ignore
weights, and EPC uses the undirected skeleton. EPC is a Monte Carlo
estimate, and cytoHubba and
[`centiserve::epc()`](https://rdrr.io/pkg/centiserve/man/epc.html)
report the same quantity multiplied by the number of runs.

## References

Latora, V., & Marchiori, M. (2001). Efficient behavior of small-world
networks. Physical Review Letters, 87(19), 198701.

Eidsaa, M., & Almaas, E. (2013). s-core network decomposition: A
generalization of k-core analysis to weighted networks. Physical Review
E, 88(6), 062819.
[doi:10.1103/PhysRevE.88.062819](https://doi.org/10.1103/PhysRevE.88.062819)
.

Borgatti, S. P. (2006). Identifying sets of key players in a social
network. Computational and Mathematical Organization Theory, 12(1),
21-34.

Sade, D. S. (1989). Sociometrics of Macaca mulatta III: n-path
centrality in grooming networks. Social Networks, 11(3), 273-292.

Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
(2008). Hubba: hub objects analyzer, a framework of interactome hubs
identification for network biology. Nucleic Acids Research, 36,
W438-W443. [doi:10.1093/nar/gkn257](https://doi.org/10.1093/nar/gkn257)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_geodesic_kpath`](https://sonsoles.me/cograph/reference/centrality_weighted_kshell.md),
[`network_local_efficiency`](https://sonsoles.me/cograph/reference/network_local_efficiency.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_local_efficiency(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.951812   3.504006   3.592003   2.642620   2.314935   3.058385   5.142414 
#>   Evaluate     Create      Share 
#>   4.300894   2.473077   4.373003 
centrality_s_core(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       0.99       1.08       1.08       0.99       0.92       0.99       0.77 
#>   Evaluate     Create      Share 
#>       1.08       1.08       1.08 
centrality_fragmentation(regulation_net, weighted = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.1944444  0.2083333  0.2222222  0.2083333  0.1944444  0.1944444  0.1805556 
#>   Evaluate     Create      Share 
#>  0.1944444  0.2083333  0.1944444 
centrality_kpath(regulation_net, kpath_len = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         37         47         58         46         36         38         30 
#>   Evaluate     Create      Share 
#>         40         48         40 
centrality_epc(regulation_net, epc_runs = 100, epc_seed = 1)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>      0.931      0.956      0.953      0.938      0.920      0.942      0.922 
#>   Evaluate     Create      Share 
#>      0.946      0.952      0.956 
```
