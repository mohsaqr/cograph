# Local Transitivity

Local transitivity, the clustering coefficient of Watts and Strogatz
(1998), is the share of pairs of a node's neighbors that are themselves
linked: \$\$C_i = \frac{2 T_i}{k_i (k_i - 1)},\$\$ where \\T_i\\ is the
number of triangles through node \\i\\ and \\k_i\\ its degree.

## Usage

``` r
centrality_transitivity(x, transitivity_type = "local", isolates = "nan", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- transitivity_type:

  One of `"local"` (default), `"global"`, `"undirected"`,
  `"localundirected"`, `"barrat"`, `"weighted"` or `"onnela"`.

- isolates:

  Value for nodes with fewer than two ties: `"nan"` (default) or
  `"zero"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `tna_network` (default `NULL`, which detects tna
  input).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Triangles are counted on the undirected skeleton and edge weights are
ignored. On a directed network \\k_i\\ is the total degree, in plus out,
so a reciprocated tie counts twice and the values can be lower than
`igraph::transitivity(type = "local")`, which uses the skeleton degree.
`"localundirected"` gives the same values as `"local"`. `"global"` and
`"undirected"` return the network-level ratio of closed to connected
triples for every node. `"barrat"` and `"weighted"` compute the weighted
coefficient of Barrat et al. (2004) and raise a
`cograph_directed_unsupported` error on directed input. `"onnela"`
computes \\(M^3)\_{ii} / (s_i^2 - \sum_j M\_{ij}^2)\\ on \\M = W +
W^{T}\\ with strengths \\s_i\\, the value
[`tna::centralities()`](https://sonsoles.me/tna/reference/centralities.html)
reports as Clustering, and it is the default for tna input. Under the
local and Barrat types a node with fewer than two ties is `NaN`, or 0
with `isolates = "zero"`.

## References

Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of
'small-world' networks. Nature, 393(6684), 440-442.
[doi:10.1038/30918](https://doi.org/10.1038/30918) .

Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A.
(2004). The architecture of complex weighted networks. Proceedings of
the National Academy of Sciences, 101(11), 3747-3752.
[doi:10.1073/pnas.0400087101](https://doi.org/10.1073/pnas.0400087101) .

## See also

[`centrality_clusterrank`](https://sonsoles.me/cograph/reference/centrality_clusterrank.md),
[`centrality_topological_coefficient`](https://sonsoles.me/cograph/reference/centrality_topological_coefficient.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_transitivity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3333333  0.3333333  0.3928571  0.3333333  0.2000000  0.4000000  0.5000000 
#>   Evaluate     Create      Share 
#>  0.5000000  0.3809524  0.4000000 
```
