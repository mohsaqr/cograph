# TOPSIS Relative-Entropy Centrality

The multi-attribute ranking method of Liu et al. (2015), TOPSIS-RE,
rates every node on four indicators, degree centrality, closeness
centrality, betweenness centrality and the improved k-shell index, and
scores it by how close it lies to the best value of each and how far
from the worst. Each indicator is divided by its Euclidean norm over the
nodes and multiplied by its weight, 0.0625, 0.1875, 0.1875 and 0.5625 in
that order, to give \\r\_{ij}\\. With \\r_j^+\\ and \\r_j^-\\ the
largest and the smallest value of indicator \\j\\, \$\$S_i^{+} =
\Bigl\\\sum\_{j} \Bigl\[r_j^{+} \log\frac{r_j^{+}}{r\_{ij}} + (1 -
r_j^{+}) \log\frac{1 - r_j^{+}}{1 - r\_{ij}}\Bigr\]\Bigr\\^{1/2}, \qquad
C_i = \frac{S_i^{-}}{S_i^{+} + S_i^{-}},\$\$ and \\S_i^-\\ is defined in
the same way with \\r_j^-\\.

## Usage

``` r
centrality_topsis_re(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order,
between 0 and 1, or `NA` at every node when the score is undefined.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The score runs from 0 to 1, and a node that is best on all four
indicators scores 1. The weights are those the source derives and
prints; they are part of the method.

A node with an indicator equal to zero while another node has a positive
value scores 0, because its distance from the best is then infinite.
Betweenness is zero for every node with a single tie, so all such nodes
score 0. The equation of the source gives this result, but the source
does not discuss it, and a figure in it shows positive scores for such
nodes without saying how they were obtained.

The measure needs a connected network of at least two nodes whose nodes
are not all alike. Otherwise every score is `NA` with a
`cograph_undefined_measure` warning.

## References

Liu, Z., Jiang, C., Wang, J. and Yu, H. (2015). The node importance in
actual complex networks based on a multi-attribute ranking method.
Knowledge-Based Systems, 84, 56-66.
[doi:10.1016/j.knosys.2015.03.026](https://doi.org/10.1016/j.knosys.2015.03.026)
.

## See also

[`centrality_iksd`](https://sonsoles.me/cograph/reference/centrality_iksd.md),
[`centrality_hybrid`](https://sonsoles.me/cograph/reference/centrality_hybrid.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
kite <- matrix(0, 6, 6, dimnames = list(letters[1:6], letters[1:6]))
kite[rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(4, 6))] <- 1
kite <- kite + t(kite)
centrality_topsis_re(kite)
#>         a         b         c         d         e         f 
#> 0.0000000 0.0000000 0.9226135 0.7338031 0.0000000 0.0000000 
```
