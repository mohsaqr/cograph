# K-Shell Hybrid Method

The k-shell hybrid method (ksh; Namtirtha et al. 2018) adds up, over the
nodes within a few steps of a node, the root of the two nodes' summed
k-shell indices and a share of the other node's degree, each divided by
the squared distance: \$\$ksh(m) = \sum\_{n:\\1 \le d\_{mn} \le r}
\frac{\sqrt{ks_m + ks_n} + \mu\\k_n}{d\_{mn}^2},\$\$ with \\ks\\ the
k-shell index, \\k\\ the degree, \\d\_{mn}\\ the distance and \\r\\ the
radius.

## Usage

``` r
centrality_ksh(x, ...)
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
`ksh_mu` is 0.4 by default, the value of the source's worked example and
of two of its tables. The authors tune it per network (0.4, 0.1 and 0.01
in the source) and recommend no single value; it must lie in \\\[0,
1\]\\. `ksh_radius` is 3 by default, the value the source recommends;
`Inf` sums over the whole component. The root covers the shell sum alone
and the distance is squared: with these defaults the implementation
reproduces the 13 values and the worked value the source prints for its
13-node example (truncated to two decimals) and the 13 values Maji et
al. (2020) print for the same network, on the edge list read from the
figures of both papers. Isolated nodes score 0. A `ksh_mu` outside
\\\[0, 1\]\\, or a `ksh_radius` that is not a whole number of at least
one or `Inf`, raises a `cograph_bad_parameter` error.
`normalized = TRUE` divides the scores by their maximum.

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

[`centrality_eskh`](https://sonsoles.me/cograph/reference/centrality_eskh.md),
[`centrality_ikh`](https://sonsoles.me/cograph/reference/centrality_ikh.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ksh(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   29.97056   33.49188   37.01320   33.19188   29.67056   30.27056   27.04924 
#>   Evaluate     Create      Share 
#>   30.87056   33.79188   30.87056 
centrality_ksh(regulation_net, ksh_mu = 0.1)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   20.22056   22.69188   25.16320   22.61688   20.14556   20.29556   17.89924 
#>   Evaluate     Create      Share 
#>   20.44556   22.76688   20.44556 
```
