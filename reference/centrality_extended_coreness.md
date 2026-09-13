# Extended neighborhood coreness

Bae and Kim's extended neighborhood coreness sums the neighborhood
coreness of every immediate neighbor: \\C\_{nc+}(i)=\sum\_{j\in
N(i)}\sum\_{l\in N(j)}k_s(l)\\. Equivalently, the score is \\A^2 k_s\\.
Here k_s is the core-number vector of the original simple undirected
graph. Core numbers are not recomputed inside each neighborhood.

## Usage

``` r
centrality_extended_coreness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  With `normalized = TRUE`, positive scores are divided by their
  maximum.

## Value

Named numeric vector in input node order.

## Details

Every length-two walk contributes its endpoint's core number, including
returns to the focal node and repeated endpoints reached via different
neighbors. This is not a sum over distinct nodes at distance two.
Isolates score zero. On a tree, it equals the sum of neighboring
degrees; on a d-regular graph it equals d cubed. A larger score means
more access to core-rich neighborhoods; numerical equivalence does not
imply superior spreading prediction for every network.

Uses the simple undirected unweighted skeleton: either direction creates
an edge, parallel edges count once and loops are removed. This
projection is a cograph convention for inputs outside the published
domain. Weights, `mode` and shortest-path weight inversion do not affect
the score.

## References

Bae, J., & Kim, S. (2014). Identifying and ranking influential spreaders
in complex networks by neighborhood coreness. Physica A, 395, 549-559.
[doi:10.1016/j.physa.2013.10.047](https://doi.org/10.1016/j.physa.2013.10.047)
. The equations used here are reproduced as equations 2 and 3 in Ma, Ma,
Zhang & Wang (2016), Physica A, 451, 205-212.
[doi:10.1016/j.physa.2015.12.162](https://doi.org/10.1016/j.physa.2015.12.162)
.

## Examples

``` r
centrality_extended_coreness(igraph::make_ring(6))
#> 1 2 3 4 5 6 
#> 8 8 8 8 8 8 
```
