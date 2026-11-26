# Extended K-Shell Hybrid Method

The extended k-shell hybrid method (Namtirtha et al. 2018, who write it
\\ks\_{h+}\\) sums the k-shell hybrid score of the neighbors of a node:
\$\$ESKH(m) = \sum\_{j \in N(m)} ksh(j),\$\$ with \\ksh\\ the score
[`centrality_ksh`](https://sonsoles.me/cograph/reference/centrality_ksh.md)
returns.

## Usage

``` r
centrality_eskh(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ksh_mu` (the weight of the neighbor's degree,
  default 0.4) and `ksh_radius` (the largest distance summed over,
  default 3).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The node's own score is not added: the source's worked example and the
values it prints for its leaves require the open neighborhood. `ksh_mu`
and `ksh_radius` are those of
[`centrality_ksh`](https://sonsoles.me/cograph/reference/centrality_ksh.md).
With the defaults the implementation reproduces the 13 values Maji et
al. (2020) print for the 13-node example and 11 of the 14 values of the
source. The other three are one quantity, which the source obtains by
adding values already truncated to two decimals (48.03, where the exact
value is 48.045). Isolated nodes score 0. A `ksh_mu` outside \\\[0,
1\]\\, or a `ksh_radius` that is not a whole number of at least one or
`Inf`, raises a `cograph_bad_parameter` error. `normalized = TRUE`
divides the scores by their maximum.

## References

Namtirtha, A., Dutta, A. and Dutta, B. (2018). Identifying influential
spreaders in complex networks based on kshell hybrid method. Physica A:
Statistical Mechanics and its Applications, 499, 310-324.
[doi:10.1016/j.physa.2018.02.016](https://doi.org/10.1016/j.physa.2018.02.016)
.

Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
Influential spreaders identification in complex networks with improved
k-shell hybrid method. Expert Systems with Applications, 144, 113092.
[doi:10.1016/j.eswa.2019.113092](https://doi.org/10.1016/j.eswa.2019.113092)
.

## See also

[`centrality_ksh`](https://sonsoles.me/cograph/reference/centrality_ksh.md),
[`centrality_eikh`](https://sonsoles.me/cograph/reference/centrality_eikh.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_eskh(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   157.7955   189.8660   218.9366   186.0447   155.1741   160.1168   133.3675 
#>   Evaluate     Create      Share 
#>   167.1594   192.4873   167.4594 
```
