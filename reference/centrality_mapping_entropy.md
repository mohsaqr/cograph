# Mapping Entropy

Mapping entropy (ME; Nie et al. 2016) weighs the logarithms of the
degree centralities of a node's neighbors by the node's own degree
centrality: \$\$ME_i = -DC_i \sum\_{j \in N(i)} \log DC_j, \qquad DC_i =
\frac{k_i}{N - 1},\$\$ with \\k\\ the degree and \\N\\ the number of
nodes.

## Usage

``` r
centrality_mapping_entropy(x, ...)
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
network, so direction, weights, loops and parallel edges are ignored.
The source cites degree centrality for \\DC\\ without giving a formula;
\\DC\\ is the degree divided by \\N - 1\\, the usual degree centrality.
Only with this reading is the source's equation (1) an entropy, and only
with it does removing the nodes in decreasing order of the score beat
removing them by degree on the two networks of the source's Table 1 that
could be checked, as the source reports. The score is then never
negative and grows with the degree. The Centrality Zoo and Omar and
Plapper (2021) use the raw degree, which makes every score zero or
negative and ranks low-degree nodes first; the two readings order the
nodes differently, not on another scale. The logarithm is natural; the
base only rescales.
[`centrality_local_entropy`](https://sonsoles.me/cograph/reference/centrality_heatmap.md)
is equation (2) of the same source with the raw degree, a different
quantity: the two have Kendall correlations between -0.56 and -0.27 on
four test networks. The score ranks nodes in nearly the reverse order of
`local_bridging` (mean Kendall correlation -0.98 over the four networks,
-0.97 at the least) and much like the degree (0.88 to 0.93).. The source
prints no score, so the implementation is checked against the definition
only. Isolated nodes, and the node of a one-node network, score 0.
`normalized = TRUE` divides the scores by their maximum.

## References

Nie, T., Guo, Z., Zhao, K. and Lu, Z. (2016). Using mapping entropy to
identify node centrality in complex networks. Physica A: Statistical
Mechanics and its Applications, 453, 290-297.
[doi:10.1016/j.physa.2016.02.009](https://doi.org/10.1016/j.physa.2016.02.009)
.

## See also

[`centrality_local_entropy`](https://sonsoles.me/cograph/reference/centrality_heatmap.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mapping_entropy(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.4301612  2.1540465  2.9483110  2.2755942  1.5697804  1.3288715  0.7333472 
#>   Evaluate     Create      Share 
#>  1.1419425  2.0052841  1.1419425 
```
