# Estrada Index

Computes the Estrada index, a graph-level spectral invariant \$\$EE(G) =
\sum\_{i=1}^{n} e^{\lambda_i}\$\$ where \\\lambda_i\\ are the
eigenvalues of the binary adjacency matrix. Edge weights are ignored.
For an undirected network the index equals \\\sum_k M_k / k!\\, where
\\M_k\\ is the number of closed walks of length \\k\\, and it is the sum
of the subgraph centralities of all nodes. For a directed network the
function sums \\e^{Re(\lambda_i)}\\ over the real parts of the
eigenvalues, which differs from the closed-walk sum when the adjacency
matrix has complex eigenvalues.

## Usage

``` r
estrada_index(x)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

## Value

Numeric scalar: the Estrada index of the graph, or 0 for a graph with no
nodes.

## References

Estrada, E. (2000). Characterization of 3D molecular structure.
*Chemical Physics Letters*, 319(5-6), 713-718.

## See also

[`centrality_subgraph`](https://sonsoles.me/cograph/reference/centrality_subgraph.md)
for the per-node measure. On an undirected network its values sum to
`estrada_index(x)`.

## Examples

``` r
estrada_index(regulation_net)
#> [1] 24.23546
```
