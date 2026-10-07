# Lhc Index

The Lhc index (Wang et al. 2021) sums over a node's neighbors \\w\\ an
influence \\C(w)\\, the degree of every node \\u\\ within distance \\d\\
of \\w\\, inflated by its triangle share and divided by the squared
distance. The triangle share is \\TP(u) = NTS(u) / TNTS\\, where
\\NTS(u)\\ counts the triangles on \\u\\ and \\TNTS = \sum_u NTS(u)\\.
\$\$Lhc(v) = \sum\_{w \in N(v)} C(w), \qquad C(w) = \sum\_{1 \le d(u,w)
\le d} \frac{k_u (1 + TP(u))}{d(u,w)^2}\$\$

## Usage

``` r
centrality_lhc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `lhc_radius` (the distance range \\d\\, default 2).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored. On
a triangle-free graph \\TP\\ is set to zero. \\TNTS\\ is a global sum,
so adding a component that carries a triangle changes every score.
Isolates score zero. A `lhc_radius` that is not a whole number of at
least one raises a `cograph_bad_parameter` error. The Centrality Zoo
(section 2.221) names the denominator the number of triangles, which is
one third of \\TNTS\\. The implementation follows the source.

## References

Wang, X., Yang, Q., Liu, M. and Ma, X. (2021). Comprehensive influence
of topological location and neighbor information on identifying
influential nodes in complex networks. PLoS ONE, 16(5), e0251208.
[doi:10.1371/journal.pone.0251208](https://doi.org/10.1371/journal.pone.0251208)
.

## See also

[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md),
[`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lhc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   190.7149   232.6096   266.9298   227.4605   188.5000   190.7851   157.4912 
#>   Evaluate     Create      Share 
#>   198.3333   233.4605   198.8289 
```
