# Distance Entropy

Shannon entropy of the distribution of hop distances from a node to
every node it can reach (Stella & De Domenico 2018), normalised so that
a uniform spread over the node's distance range scores 1: \$\$h(i) =
-\frac{1}{\log(M_i - m_i + 1)} \sum\_{k = m_i}^{M_i} p_k^{(i)} \log
p_k^{(i)}, \qquad p_k^{(i)} = n_k^{(i)} / R_i,\$\$ where \\n_k^{(i)}\\
is the number of nodes at distance \\k\\ from \\i\\, \\R_i\\ the number
of reachable nodes, and \\m_i, M_i\\ the minimum and maximum distance.
High values mark nodes whose reach is spread evenly across many network
layers; a node whose reachable nodes all sit at one distance scores 0.
Closeness summarises the mean of the same distribution; distance entropy
summarises its spread.

## Usage

``` r
centrality_distance_entropy(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"out"` (distances along
  out-edges), or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node, in \[0, 1\]. `NaN` for a node
that reaches no other node.

## Details

Distances are hop counts (edge weights are ignored). The original paper
normalises by \\\log(M_i - m_i)\\, which is undefined when only two
distinct distances occur; \\\log(M_i - m_i + 1)\\ is used here so the
index is bounded by 1 for a uniform distribution.

## References

Stella, M., & De Domenico, M. (2018). Distance entropy cartography
characterises centrality in complex networks. Entropy, 20(4), 268.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md)
for the growth-rate view of the same distance profile.

## Examples

``` r
path4 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
rownames(path4) <- colnames(path4) <- c("A", "B", "C", "D")
centrality_distance_entropy(path4)
#>         A         B         C         D 
#> 1.0000000 0.9182958 0.9182958 1.0000000 
```
