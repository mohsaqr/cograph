# TOPSIS Centrality

TOPSIS centrality (Du et al. 2014) ranks nodes by their closeness to an
ideal node on three centrality measures at once. Each measure is divided
by its Euclidean norm over the nodes and multiplied by its weight; the
ideal node has the largest value on every measure and the anti-ideal
node the smallest. With \\S^+\_i\\ and \\S^-\_i\\ the Euclidean
distances of node \\i\\ to the two, \$\$C_i = \frac{S^-\_i}{S^-\_i +
S^+\_i}.\$\$ The measures are degree, closeness and either betweenness
or eigenvector centrality.

## Usage

``` r
centrality_topsis(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `topsis_attribute` (`\"betweenness\"` or
  `\"eigenvector\"`) and `topsis_weights` (three weights, default one
  third each).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`topsis_attribute` names the third measure: `"betweenness"` (the
default), which the source uses where traffic follows shortest paths, or
`"eigenvector"`, which it uses where traffic spreads along walks.
`topsis_weights` holds the weights of degree, closeness and the third
measure. The default is one third each: the source states no weights for
networks, and uses equal weights in its only numerical illustration.
Only the proportions of the weights matter. The score lies in \\\[0,
1\]\\. It has no value on a disconnected network, where closeness is
undefined, on a one-node network, and on a network whose nodes are all
equal on the three measures (a measure whose range is within 1e-12 of
its largest value counts as equal); there the measure warns (class
`cograph_undefined_measure`) and returns `NA`. The source prints no
score of a network node; the implementation reproduces the distances and
the ranking of its numerical illustration.
[`centrality_topsis_re`](https://sonsoles.me/cograph/reference/centrality_topsis_re.md)
is a later variant with four measures, fixed weights and a
relative-entropy distance. The weighted TOPSIS of Hu et al. (2016) is
this measure with the betweenness and weights taken from a simulated
spreading process; given those weights through `topsis_weights`, it
gives the same scores. Its weight rule is not built in: the spreading
values it prints do not follow from the model it states, and the weights
it gives change strongly from one simulation to the next. With the
weights its worked example multiplies into the attributes,
`topsis_weights = c(0.5824, 0.0705, 0.3471)`, the implementation
reproduces the six scores and the ranking of its Table 2; the example
prints these weights in the order degree, betweenness, closeness but
applies them in the order degree, closeness, betweenness. An unknown
`topsis_attribute`, or a `topsis_weights` that is not three finite
nonnegative numbers, not all zero, raises a `cograph_bad_parameter`
error. `normalized = TRUE` divides the scores by their maximum.

## References

Du, Y., Gao, C., Hu, Y., Mahadevan, S. and Deng, Y. (2014). A new method
of identifying influential nodes in complex networks based on TOPSIS.
Physica A: Statistical Mechanics and its Applications, 399, 57-69.
[doi:10.1016/j.physa.2013.12.031](https://doi.org/10.1016/j.physa.2013.12.031)
.

Hu, J., Du, Y., Mo, H., Wei, D. and Deng, Y. (2016). A modified weighted
TOPSIS to identify influential nodes in complex networks. Physica A:
Statistical Mechanics and its Applications, 444, 73-85.
[doi:10.1016/j.physa.2015.09.028](https://doi.org/10.1016/j.physa.2015.09.028)
.

## See also

[`centrality_topsis_re`](https://sonsoles.me/cograph/reference/centrality_topsis_re.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_topsis(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3203679  0.7310498  0.9747116  0.8595407  0.5758228  0.3317009  0.0000000 
#>   Evaluate     Create      Share 
#>  0.2230220  0.5622354  0.1898842 
centrality_topsis(regulation_net, topsis_attribute = "eigenvector")
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3000748  0.6619468  1.0000000  0.6300121  0.2845016  0.3129138  0.0000000 
#>   Evaluate     Create      Share 
#>  0.3655761  0.6764735  0.3690224 
```
