# NL Centrality

NL centrality (Shao et al. 2019) adds, for each neighbor of a node, the
size of the neighbor's two-step neighborhood and the importance of the
tie to it: \$\$NL(v) = \sum\_{u \in N(v)} \Bigl(\phi(u) + I\_{uv}
\frac{k_v - 1}{k_v + k_u - 2}\Bigr),\$\$ where \\\phi(u)\\ is the number
of nodes within two steps of \\u\\, \\k\\ the degree and \\I\_{uv}\\ the
importance of the tie, \\(k_u - t - 1)(k_v - t - 1) / (t/2 + 1)\\ with
\\t\\ the number of triangles on it.

## Usage

``` r
centrality_nl(x, ...)
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

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The second term is the tie contribution of
[`centrality_dil`](https://sonsoles.me/cograph/reference/centrality_dil.md),
so the score equals the sum of \\\phi\\ over the neighbors plus that
measure minus the degree. \\\phi(u)\\ does not count \\u\\ itself: the
wording and the example of the source list other nodes only, and its
tables were not available to test this. The first term carries most of
the score. The share of an isolated tie is 0/0 and is taken as 0.
Isolated nodes score 0. No published value checks the implementation.
`normalized = TRUE` divides the scores by their maximum.

## References

Shao, Z., Liu, S., Zhao, Y. and Liu, Y. (2019). Identifying influential
nodes in complex networks based on Neighbours and edges. Peer-to-Peer
Networking and Applications, 12, 1528-1537.
[doi:10.1007/s12083-018-0681-x](https://doi.org/10.1007/s12083-018-0681-x)
.

## See also

[`centrality_dil`](https://sonsoles.me/cograph/reference/centrality_dil.md),
[`centrality_lsc`](https://sonsoles.me/cograph/reference/centrality_lsc.md),
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_nl(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   51.86667   65.58081   69.10182   70.64646   57.88571   56.64444   41.71429 
#>   Evaluate     Create      Share 
#>   54.22222   60.70202   49.50222 
```
