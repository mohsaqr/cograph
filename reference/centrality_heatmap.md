# Heatmap, Flow Coefficient, Local Entropy, Weighted h-index and Redundancy

Five local measures. Heatmap centrality (Duron 2020) is the farness of a
node minus the mean farness of its neighbors, \\C(v) = f(v) -
\frac{1}{k_v} \sum\_{u \in N(v)} f(u)\\, with \\f\\ the sum of hop
distances to the reachable nodes. The flow coefficient (Honey et al.
2007) is the fraction of ordered pairs of distinct neighbors joined by a
two-step path through the node and by no direct link, as in the Brain
Connectivity Toolbox. Local entropy (Nie et al. 2016) is \\-\sum\_{j \in
N(i)} k_j \ln k_j\\. The weighted h-index (Gao et al. 2019) is the
h-index of the multiset in which each neighbor \\j\\ contributes the
value \\k_i k_j\\ repeated \\k_j\\ times. Redundancy (Burt 1992;
Borgatti 1997) is the mean degree of the neighbors of a node within its
ego network.

## Usage

``` r
centrality_heatmap(x, mode = "all", ...)

centrality_flow_coefficient(x, ...)

centrality_local_entropy(x, mode = "all", ...)

centrality_weighted_h_index(x, mode = "all", ...)

centrality_redundancy(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order. The
weighted h-index is an integer vector.

## Details

Edge weights are ignored by all five. Heatmap, local entropy and the
weighted h-index follow `mode`. Redundancy ignores direction, and the
flow coefficient uses the direction of the links. On an undirected
network the flow coefficient of a node with at least two neighbors
equals one minus its clustering coefficient, and redundancy equals
degree minus effective size. Lower values mark more central nodes for
heatmap and local entropy. Isolated nodes return `NaN` for heatmap and 0
for local entropy, and nodes with fewer than two neighbors score 0 on
the flow coefficient. The formula for local entropy follows the
Centrality Zoo and the survey of Omar and Plapper (2021), which agree.

## References

Duron, C. (2020). Heatmap centrality: A new measure to identify super-
spreader nodes in scale-free networks. PLOS ONE, 15(7), e0235690.

Honey, C. J., Kotter, R., Breakspear, M., & Sporns, O. (2007). Network
structure of cerebral cortex shapes functional connectivity on multiple
time scales. PNAS, 104(24), 10240-10245.

Nie, T., Guo, Z., Zhao, K., & Lu, Z.-M. (2016). Using mapping entropy to
identify node centrality in complex networks. Physica A, 453, 290-297.

Gao, L., Yu, S., Li, M., Shen, Z., & Gao, Z. (2019). Weighted h-index
for identifying influential spreaders. Symmetry, 11(10), 1263.

Borgatti, S. P. (1997). Structural holes: Unpacking Burt's redundancy
measures. Connections, 20(1), 35-38.

## See also

[`centrality_effective_size`](https://sonsoles.me/cograph/reference/centrality_effective_size.md),
[`centrality_transitivity`](https://sonsoles.me/cograph/reference/centrality_transitivity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_heatmap(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.4000000 -0.6666667 -1.7142857 -0.8333333  0.2000000  0.6000000  2.0000000 
#>   Evaluate     Create      Share 
#>  1.0000000 -0.5000000  1.0000000 
centrality_flow_coefficient(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2500000  0.2333333  0.1904762  0.3000000  0.2000000  0.2000000  0.2500000 
#>   Evaluate     Create      Share 
#>  0.2000000  0.2333333  0.3000000 
centrality_local_entropy(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  -45.64268  -54.05867  -61.93842  -51.35531  -43.30812  -48.34605  -43.16967 
#>   Evaluate     Create      Share 
#>  -53.92023  -56.56069  -53.92023 
centrality_weighted_h_index(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         25         28         33         27         22         25         20 
#>   Evaluate     Create      Share 
#>         25         30         25 
centrality_redundancy(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.000000   2.333333   3.142857   1.666667   1.200000   1.600000   1.500000 
#>   Evaluate     Create      Share 
#>   2.000000   2.666667   2.400000 
```
