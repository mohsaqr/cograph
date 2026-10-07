# Extended Neighborhood Coreness

Extended neighborhood coreness (Bae and Kim 2014) sums the neighborhood
coreness of every neighbor of a node: \$\$C\_{nc+}(i) = \sum\_{j \in
N(i)} \sum\_{l \in N(j)} k_s(l),\$\$ where \\k_s\\ is the core number in
the whole network. The score equals \\A^2 k_s\\.

## Usage

``` r
centrality_extended_coreness(x, ...)
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

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored. Every walk of length two adds the
core number of its endpoint, including walks that return to the focal
node. Isolated nodes score 0. On a tree the score equals the sum of the
neighbors' degrees, and on a \\d\\-regular network it equals \\d^3\\.

## References

Bae, J., & Kim, S. (2014). Identifying and ranking influential spreaders
in complex networks by neighborhood coreness. Physica A, 395, 549-559.
[doi:10.1016/j.physa.2013.10.047](https://doi.org/10.1016/j.physa.2013.10.047)
.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_extended_gravity`](https://sonsoles.me/cograph/reference/centrality_extended_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_extended_coreness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        108        128        148        124        104        112         96 
#>   Evaluate     Create      Share 
#>        120        132        120 
```
