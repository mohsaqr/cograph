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

Triangles and degrees are counted on the undirected skeleton, so a
reciprocated tie counts once, and edge weights are ignored. The values
equal `igraph::transitivity(type = "local")` on directed and undirected
networks. `"localundirected"` gives the same values as `"local"`, as in
igraph. `"global"` and `"undirected"` return the network-level ratio of
closed to connected triples for every node. `"barrat"` and `"weighted"`
compute the weighted coefficient of Barrat et al. (2004) and raise a
`cograph_directed_unsupported` error on directed input. `"onnela"`
computes the weighted clustering coefficient of Zhang and Horvath
(2005), \\(M^3)\_{ii} / (s_i^2 - \sum_j M\_{ij}^2)\\ on \\M = W +
W^{T}\\ with strengths \\s_i\\. This is the value
[`tna::centralities()`](https://sonsoles.me/tna/reference/centralities.html)
reports as Clustering, and it is the default for tna input. The option
keeps the name `"onnela"` used by tna, but the formula multiplies the
raw weights of a triangle and differs from the geometric-mean form of
Onnela et al. (2005). Under the local and Barrat types a node with fewer
than two neighbors is `NaN`, or 0 with `isolates = "zero"`.

## References

Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of
'small-world' networks. Nature, 393(6684), 440-442.
[doi:10.1038/30918](https://doi.org/10.1038/30918) .

Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A.
(2004). The architecture of complex weighted networks. Proceedings of
the National Academy of Sciences, 101(11), 3747-3752.
[doi:10.1073/pnas.0400087101](https://doi.org/10.1073/pnas.0400087101) .

Zhang, B., & Horvath, S. (2005). A general framework for weighted gene
co-expression network analysis. Statistical Applications in Genetics and
Molecular Biology, 4(1), Article 17.
[doi:10.2202/1544-6115.1128](https://doi.org/10.2202/1544-6115.1128) .

Onnela, J.-P., Saramaki, J., Kertesz, J., & Kaski, K. (2005). Intensity
and coherence of motifs in weighted complex networks. Physical Review E,
71(6), 065103.
[doi:10.1103/PhysRevE.71.065103](https://doi.org/10.1103/PhysRevE.71.065103)
.

## See also

[`centrality_clusterrank`](https://sonsoles.me/cograph/reference/centrality_clusterrank.md),
[`centrality_topological_coefficient`](https://sonsoles.me/cograph/reference/centrality_topological_coefficient.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_transitivity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5000000  0.4666667  0.5238095  0.3333333  0.3000000  0.4000000  0.5000000 
#>   Evaluate     Create      Share 
#>  0.5000000  0.5333333  0.6000000 
```
