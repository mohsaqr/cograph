# Trust-PageRank

Trust-PageRank (Sheng et al. 2020) is a damped PageRank in which a node
passes its score to a neighbor in proportion to a trust value. The trust
value mixes the SimRank similarity \\s\\ of the two nodes with a degree
ratio: \$\$T(i,j) = (1-k)\frac{s(i,j)}{\sum\_{l \in N_j} s(j,l)} +
k\\\frac{d_i}{\sum\_{l \in N_j} d_l}, \qquad TPR_i =
\frac{1-\alpha}{n} + \alpha \sum\_{j \in N_i} T(i,j)\\TPR_j .\$\$

## Usage

``` r
centrality_trust_pagerank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `tpr_alpha` (damping factor, default 0.85), `tpr_k`
  (weight of the degree ratio, default 0.85), `tpr_decay` (SimRank decay
  constant, default 1), `tpr_tol` (relative tolerance, default `1e-14`)
  and `tpr_max_iter` (default 1000).

## Value

A named numeric vector with one score per node, in input node order. The
scores sum to one on a network with no isolated node and no undefined
component.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The similarity recursion runs over adjacent pairs and is anchored by
triangles. Every node of a component that has edges but no triangle is
returned as `NA` with a `cograph_undefined_measure` warning, and an
isolated node scores \\(1-\alpha)/n\\. Both the similarity and the score
iterations stop on a relative tolerance, and an iteration that reaches
`tpr_max_iter` raises `cograph_no_converge`. The Centrality Zoo
attributes the measure to Sheng et al., Physica A 541:123262, which
defines a different index. The measure is defined in the Algorithms
paper cited below.

## References

Sheng, J., Zhu, J., Wang, Y., Wang, B. and Hou, Z. (2020). Identifying
Influential Nodes of Complex Networks Based on Trust-Value. Algorithms,
13(11), 280. [doi:10.3390/a13110280](https://doi.org/10.3390/a13110280)
.

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_trust_pagerank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.08646318 0.11421066 0.14691654 0.11177852 0.08427434 0.08838884 0.06660069 
#>   Evaluate     Create      Share 
#> 0.09237486 0.11655236 0.09244001 
```
