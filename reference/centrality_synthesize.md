# Synthesize Centrality

Synthesize centrality (SC; Liu et al. 2013) adds the normalised degree
and betweenness of a node and divides by its normalised farness:
\$\$SC(v) = \frac{d_d(v) + d_b(v)}{d_c(v)}, \qquad d_d(v) =
\frac{k_v}{n - 1}, \qquad d_b(v) = \frac{2 B(v)}{(n - 1)^2}, \qquad
d_c(v) = \frac{\sum_i l(i, v)}{(n - 1)^2},\$\$ with \\k\\ the degree,
\\B\\ the betweenness and \\l\\ the distance, 1000 for a pair that
cannot reach each other.

## Usage

``` r
centrality_synthesize(x, ...)
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
\\B\\ is the betweenness
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
gives on an unweighted network, the share of shortest paths through
\\v\\ summed over unordered pairs, so \\2B\\ sums over ordered pairs as
the prose of the source's equation (7) does; its case formula says that
the path does not contain \\v\\, which contradicts that prose. The
primes of the source's equation (9), the "normalized" quantities, are
taken as equations (6)-(8) themselves, with no second rescaling. The
Centrality Zoo squares two terms and divides by the closeness; the
source does neither. The distance 1000 for an unreachable pair is the
source's, so disconnected networks have a value; an isolated node scores
0. On a one-node network the measure warns (class
`cograph_undefined_measure`) and returns `NA`. The source prints no
value that can be checked, so the implementation is checked against the
definition only. The score ranks nodes much like
[`centrality_topsis`](https://sonsoles.me/cograph/reference/centrality_topsis.md)
(Kendall correlation 0.88 to 0.97 on four test networks, mean 0.94) and
[`centrality_ahp`](https://sonsoles.me/cograph/reference/centrality_ahp.md)
(0.98 on the karate club network). `normalized = TRUE` divides the
scores by their maximum.

## References

Liu, H., Yu, X. and Lu, J. (2013). Identifying top-n opinion leaders on
local social network. IET International Conference on Smart and
Sustainable City 2013 (ICSSC 2013), 268-271.
[doi:10.1049/cp.2013.1970](https://doi.org/10.1049/cp.2013.1970) .

## See also

[`centrality_topsis`](https://sonsoles.me/cograph/reference/centrality_topsis.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_synthesize(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   3.676923   4.902778   6.260606   5.000000   3.794872   3.682051   2.664286 
#>   Evaluate     Create      Share 
#>   3.630769   4.819444   3.612821 
```
