# Estrada Index

A graph-level spectral invariant derived from subgraph centrality:
\$\$EE(G) = \sum\_{i=1}^{n} e^{\lambda_i}\$\$ where \\\lambda_i\\ are
the eigenvalues of the adjacency matrix. The Estrada index equals the
total number of closed walks in the graph, weighted by walk length:
\\EE(G) = \sum_k M_k / k!\\ where \\M_k\\ is the number of closed walks
of length \\k\\. It is the sum of subgraph centralities across all
nodes.

## Usage

``` r
estrada_index(x)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

## Value

A single numeric value — the Estrada index of the graph.

## Details

Matches `networkx.estrada_index` at machine epsilon (max relative
difference ~5e-15 across random test graphs).

## References

Estrada, E. (2000). Characterization of 3D molecular structure.
*Chemical Physics Letters*, 319(5-6), 713-718.

## See also

[`centrality_subgraph`](https://sonsoles.me/cograph/reference/centrality_subgraph.md)
for the per-node equivalent (sum of `subgraph_centrality(x)` equals
`estrada_index(x)`).

## Examples

``` r
# Karate club
g <- igraph::make_graph("Zachary")
estrada_index(g)
#> [1] 1041.247
```
