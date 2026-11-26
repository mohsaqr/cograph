# PageRank Centrality

PageRank (Brin and Page 1998) is the stationary distribution of a random
walk that follows an out-edge with probability \\d\\, choosing edges in
proportion to their weights, and otherwise jumps to a node drawn from
the reset distribution \\p\\: \$\$PR = (1 - d)\\p + d\\P^{T} PR,\$\$
where \\P\\ is the row-normalized weight matrix and \\d\\ is `damping`.

## Usage

``` r
centrality_pagerank(x, damping = 0.85, personalized = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- damping:

  Probability \\d\\ of following an edge (default 0.85).

- personalized:

  Reset distribution \\p\\, a non-negative numeric vector with one entry
  per node, named by node or in input node order. The default `NULL`
  gives the uniform distribution.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

`weighted = FALSE` gives every edge weight one. A node without out-edges
passes its score to the reset distribution. The scores sum to one and
equal
[`igraph::page_rank()`](https://r.igraph.org/reference/page_rank.html).
The vector `personalized` is rescaled to sum to one. A named vector is
matched to the node names, and its names must be the node names, each
used once; an unnamed vector is matched by position. A negative weight
raises a `cograph_negative_weights` error, an invalid `personalized` a
`cograph_bad_input` error, and a `damping` outside \\\[0, 1\]\\ a
`cograph_bad_parameter` error.

EigenTrust (Kamvar, Schlosser and Garcia-Molina 2003) with mixing weight
\\a\\ is `damping = 1 - a` with the pre-trust distribution as
`personalized`, on a network without loops whose weights are the
nonnegative local trust values (in both, a node that trusts nobody
passes its trust to the pre-trust distribution). Seeley's index (Seeley
1949, as restated by Vigna 2016), the left eigenvector of the
row-normalized matrix for the eigenvalue 1 scaled to sum 1, is
`damping = 1` on a network in which every node has an out-edge and the
walk has a single closed class that is aperiodic, such as a connected
undirected network that is not bipartite. Outside that class the result
is not Seeley's index: the iteration starts from the uniform vector and
stops after 1000 sweeps without a warning, so with more than one closed
class it returns whichever of the several stationary vectors that start
leads to and on a periodic network such as a star one of the vectors it
alternates between, and a node without out-edges, for which Seeley's
index is not defined, sends its score to the reset distribution. On a
slowly mixing network, inside the class too, the 1000 sweeps can end
short of convergence.

## References

Brin, S., & Page, L. (1998). The anatomy of a large-scale hypertextual
Web search engine. Computer Networks and ISDN Systems, 30(1-7), 107-117.
[doi:10.1016/S0169-7552(98)00110-X](https://doi.org/10.1016/S0169-7552%2898%2900110-X)
.

Kamvar, S. D., Schlosser, M. T. and Garcia-Molina, H. (2003). The
EigenTrust algorithm for reputation management in P2P networks. In
Proceedings of the 12th International Conference on World Wide Web (WWW
'03), 640-651.
[doi:10.1145/775152.775242](https://doi.org/10.1145/775152.775242) .

Seeley, J. R. (1949). The net of reciprocal influence; a problem in
treating sociometric data. Canadian Journal of Psychology, 3(4),
234-240. [doi:10.1037/h0084096](https://doi.org/10.1037/h0084096) .

Vigna, S. (2016). Spectral ranking. Network Science, 4(4), 433-445.
[doi:10.1017/nws.2016.21](https://doi.org/10.1017/nws.2016.21) .

## See also

[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_leaderrank`](https://sonsoles.me/cograph/reference/centrality_leaderrank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_pagerank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.11846147 0.03640953 0.18376724 0.12356096 0.12513119 0.06803638 0.03760071 
#>   Evaluate     Create      Share 
#> 0.07386400 0.13821279 0.09495573 
```
