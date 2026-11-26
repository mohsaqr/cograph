# Classified Neighbors Centrality

Classified neighbors centrality (Li et al. 2018) is a weighted degree in
which each neighbor counts according to where it stands in the k-shell
decomposition relative to the node: \$\$k\_{CN}(i) = \alpha\\e_u +
\beta\\e\_{eu} + \gamma\\e\_{el} + \mu\\e_l,\$\$ where \\e_u\\ counts
the neighbors in a higher shell, \\e\_{eu}\\ those in the same shell
removed in the same step or later, \\e\_{el}\\ those in the same shell
removed earlier, and \\e_l\\ those in a lower shell.

## Usage

``` r
centrality_classified_neighbors(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `cn_weights` (the weights of the four classes of
  neighbors).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The decomposition removes nodes in batches, and the batch number is the
layer of the `"onion"` measure of
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md). A
neighbor removed in the same batch counts as "same shell, later": the
table of the source's example classifies it so, while its algorithm
listing would leave it out. `cn_weights` holds the four weights in the
order of the formula. The default is `c(0.4, 0.35, 0.25, 0.1)`, equation
(4) of the source; the source then divides them by their sum, which
changes the scores by one constant and no rank. Isolated nodes score 0.
The implementation reproduces the class of every neighbor in the
source's 14-node example. A `cn_weights` that is not four finite
nonnegative numbers, not all zero, raises a `cograph_bad_parameter`
error. `normalized = TRUE` divides the scores by their maximum.

## References

Li, C., Wang, L., Sun, S. and Xia, C. (2018). Identification of
influential spreaders based on classified neighbors in real-world
complex networks. Applied Mathematics and Computation, 320, 512-523.
[doi:10.1016/j.amc.2017.10.001](https://doi.org/10.1016/j.amc.2017.10.001)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_classified_neighbors(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       1.65       1.80       2.15       1.70       1.65       1.65       1.40 
#>   Evaluate     Create      Share 
#>       1.65       1.80       1.65 
```
