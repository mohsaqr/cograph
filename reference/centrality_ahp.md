# Analytic Hierarchy Process Centrality

Analytic hierarchy process centrality (AHP; Bian et al. 2017) adds the
shares a node holds of the degree, the betweenness and the closeness of
the network, each discounted by the share of nodes where that measure is
not zero: \$\$AHP(i) = \sum\_{j=1}^{3} w_j \frac{N^+\_j}{N}
\frac{x\_{ij}}{\sum_l x\_{lj}},\$\$ with \\x\_{i1}\\ the degree,
\\x\_{i2}\\ the betweenness, \\x\_{i3}\\ the closeness (the reciprocal
of the sum of distances), \\N^+\_j\\ the number of nodes where \\x\_{lj}
\> 0\\ and \\w\\ the weights.

## Usage

``` r
centrality_ahp(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ahp_weights` (the weights of degree, betweenness and
  closeness, default one third each).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
This is the computation of the source's worked example: for each measure
a matrix of the ratios between the nodes, with 0 where either value is
0, whose priority vector is the column-normalised row mean, which gives
the formula above. The source's equation (10) names the principal
eigenvector instead, which its printed numbers do not follow. The source
derives the weights from a simulated spreading process whose printed
values cannot be regenerated; evaluated as the source evaluates it, its
weight rule gives weights within a few thousandths of one third for any
realistic spreading count. `ahp_weights` holds the weights of degree,
betweenness and closeness, one third each by default, and uses them as
given. Each share does not depend on the scale of its measure, so the
normalisation of the betweenness does not matter. Closeness has no value
on a disconnected or a one-node network; the measure then warns (class
`cograph_undefined_measure`) and returns `NA`. With
`ahp_weights = c(1, 0, 0)` and `c(0, 0, 1)` the implementation
reproduces the 20 shares the source prints for the degree and the
closeness of the kite network, and with the source's weights its ranking
of Table 3. The betweenness matrix the source prints is not that of the
kite (two nodes that are symmetric in the kite differ in it), so its
betweenness shares and final scores are not reproduced; the final scores
differ by at most 0.013. The score ranks nodes much like
[`centrality_topsis`](https://sonsoles.me/cograph/reference/centrality_topsis.md)
(Kendall correlation 0.96 to 0.995 on four test networks, mean 0.975)
and
[`centrality_relative_entropy`](https://sonsoles.me/cograph/reference/centrality_relative_entropy.md)
(mean 0.94, 0.992 on one network). A weight that is negative or not
finite, or a `ahp_weights` that is not three numbers, raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Bian, T., Hu, J. and Deng, Y. (2017). Identifying influential nodes in
complex networks based on AHP. Physica A: Statistical Mechanics and its
Applications, 479, 422-436.
[doi:10.1016/j.physa.2017.02.085](https://doi.org/10.1016/j.physa.2017.02.085)
.

## See also

[`centrality_topsis`](https://sonsoles.me/cograph/reference/centrality_topsis.md),
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ahp(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.08896395 0.11664510 0.13555447 0.12744757 0.10316148 0.08958123 0.06660409 
#>   Evaluate     Create      Share 
#> 0.08340839 0.10738584 0.08124790 
centrality_ahp(to_undirected(regulation_net), ahp_weights = c(0.5, 0.25, 0.25))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.08987111 0.11526160 0.13407326 0.12336345 0.10051926 0.09033407 0.06847158 
#>   Evaluate     Create      Share 
#> 0.08570444 0.10831716 0.08408407 
```
