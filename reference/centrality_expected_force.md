# Expected Force Centrality

The Expected Force (Lawyer 2015, equation 1) is the entropy of the
onward spreading potential after two transmission events from a seed
node. Each ordered sequence of two transmissions gives an infected
cluster of three nodes with \\D_k\\ edges to susceptible nodes, and with
natural logarithms \$\$ExF_i = -\sum_k \frac{D_k}{\sum_l D_l} \log
\frac{D_k}{\sum_l D_l}.\$\$

## Usage

``` r
centrality_expected_force(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple unweighted network with direction
kept, so weights, loops and parallel edges are ignored. In a directed
network only outgoing arcs transmit and count toward \\D_k\\. Different
orders of the same two infections count as distinct sequences. A node
that cannot start two transmissions scores zero, so isolated nodes and
nodes of components with at most three nodes score zero. When every
cluster has no edge to a susceptible node the entropy is undefined, and
the score is set to zero.

## References

Lawyer, G. (2015). Understanding the influence of all nodes in a
network. Scientific Reports, 5, 8665.
[doi:10.1038/srep08665](https://doi.org/10.1038/srep08665) .

## See also

[`centrality_modified_expected_force`](https://sonsoles.me/cograph/reference/centrality_modified_expected_force.md),
[`centrality_expected`](https://sonsoles.me/cograph/reference/centrality_expected.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_expected_force(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.757135   3.519437   2.075442   2.616095   1.584171   2.584102   2.661920 
#>   Evaluate     Create      Share 
#>   2.545039   3.025467   2.689061 
```
