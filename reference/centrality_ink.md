# Improved Neighbors' k-core

The improved neighbors' k-core (INK; Lin et al. 2014) sums a power of
the core number of every neighbor of a node: \$\$S(i) = \sum\_{j \in
J(i)} k_s(j)^{\alpha},\$\$ where \\k_s(j)\\ is the core number of \\j\\
in the whole network and \\J(i)\\ is the set of neighbors of \\i\\. It
separates nodes that share the same core number by the cores their
neighbors belong to.

## Usage

``` r
centrality_ink(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ink_alpha` (the exponent \\\alpha\\, default 1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The node's own core number is not part of its sum. Scores are raw sums.
Isolated nodes score 0, the empty sum.

With `ink_alpha = 0` the score is the degree. With `ink_alpha = 1`, the
default, it is the sum of the neighbors' core numbers, which is the
neighborhood coreness of Bae and Kim (2014). The default is the value
Lin et al. use in their experiments. They report that the
best-performing exponent depends on the network (0.7, 1, 2.4 and 5 on
their four networks). Values above one give more weight to neighbors in
high cores, and values below one give less.

A negative `ink_alpha`, which the source does not define, raises a
`cograph_bad_parameter` error, and so does an exponent so large that a
core number raised to it is not a finite number. `normalized = TRUE`
divides the scores by their maximum.

## References

Lin, J.-H., Guo, Q., Dong, W.-Z., Tang, L.-Y. and Liu, J.-G. (2014).
Identifying the node spreading influence with largest k-core values.
Physics Letters A, 378(45), 3279-3284.
[doi:10.1016/j.physleta.2014.09.054](https://doi.org/10.1016/j.physleta.2014.09.054)
.

Bae, J. and Kim, S. (2014). Identifying and ranking influential
spreaders in complex networks by neighborhood coreness. Physica A:
Statistical Mechanics and its Applications, 395, 549-559.
[doi:10.1016/j.physa.2013.10.047](https://doi.org/10.1016/j.physa.2013.10.047)
.

## See also

[`centrality_extended_coreness`](https://sonsoles.me/cograph/reference/centrality_extended_coreness.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ink(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         20         24         28         24         20         20         16 
#>   Evaluate     Create      Share 
#>         20         24         20 
centrality_ink(regulation_net, ink_alpha = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         80         96        112         96         80         80         64 
#>   Evaluate     Create      Share 
#>         80         96         80 
```
