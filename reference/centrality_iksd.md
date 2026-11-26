# Improved k-shell Decomposition

The improved k-shell decomposition (IKSD; Liu et al. 2015) removes, in
each iteration, the nodes whose current degree is the lowest in what
remains of the network, and gives them the number of the iteration. A
node removed late lies deep in the network.

## Usage

``` r
centrality_iksd(x, ...)
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

A named numeric vector with one iteration number per node, in input node
order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Only the nodes at the lowest degree leave in an iteration. Nodes that
the removal brings down to that degree wait for the next iteration,
which separates nodes that share a k-shell. The decomposition runs over
the whole network at once, so isolated nodes form the first iteration
and raise every other label by one. `normalized = TRUE` divides the
labels by their maximum.

## References

Liu, Z., Jiang, C., Wang, J. and Yu, H. (2015). The node importance in
actual complex networks based on a multi-attribute ranking method.
Knowledge-Based Systems, 84, 56-66.
[doi:10.1016/j.knosys.2015.03.026](https://doi.org/10.1016/j.knosys.2015.03.026)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_gli`](https://sonsoles.me/cograph/reference/centrality_gli.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_iksd(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          3          5          5          4          2          3          1 
#>   Evaluate     Create      Share 
#>          3          5          5 
```
