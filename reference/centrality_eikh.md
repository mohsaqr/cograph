# Extended Improved K-Shell Hybrid Method

The extended improved k-shell hybrid method (Maji et al. 2020, who write
it \\ks\_{hi+}\\) sums the improved k-shell hybrid score of the
neighbors of a node: \$\$EIKH(i) = \sum\_{j \in N(i)} IKH(j),\$\$ with
\\IKH\\ the score
[`centrality_ikh`](https://sonsoles.me/cograph/reference/centrality_ikh.md)
returns.

## Usage

``` r
centrality_eikh(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ksh_radius` (the largest distance summed over,
  default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The node's own score is not added, as the algorithm and the worked
example of the source have it. `ksh_radius` is that of
[`centrality_ikh`](https://sonsoles.me/cograph/reference/centrality_ikh.md).
The implementation reproduces the 13 values of the source's Table 1 and
its worked value, on the 13-node network of Namtirtha et al. (2018).
Isolated nodes score 0. A `ksh_radius` that is not a finite whole number
of at least one raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
Influential spreaders identification in complex networks with improved
k-shell hybrid method. Expert Systems with Applications, 144, 113092.
[doi:10.1016/j.eswa.2019.113092](https://doi.org/10.1016/j.eswa.2019.113092)
.

## See also

[`centrality_ikh`](https://sonsoles.me/cograph/reference/centrality_ikh.md),
[`centrality_eskh`](https://sonsoles.me/cograph/reference/centrality_eskh.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_eikh(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   207.9167   250.1667   288.2500   244.7500   204.1667   211.2500   176.5000 
#>   Evaluate     Create      Share 
#>   221.2500   253.9167   221.6667 
```
