# KED method centrality

The KED method of Chen, Xiao, Zeng and Zhang combines how many local
paths leave a node with how diverse they are:
\\KED(i)=k_i\\(1+H_i)\\\exp(K_i/N)\\, where \\K_i=\sum\_{j\in N(i)}k_j\\
is the sum of the neighbours' degrees, \\H_i=\bigl(\sum\_{j\in
N(i)}-p_j\log p_j\bigr)/\log k_i\\ with \\p_j=k_j/K_i\\ is the
normalised entropy of the neighbour-degree distribution, and \\N\\ is
the number of nodes in the whole graph. The source calls \\K_i\\ the
local path number and \\H_i\\ the path diversity: two nodes of equal
degree with equally many second neighbours are separated by how evenly
their neighbours carry those paths.

## Usage

``` r
centrality_ked(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

\\H_i\\ is a ratio of two logarithms in the same base – equation (2)
divides the entropy by the entropy of the uniform distribution on
\\k_i\\ outcomes – so the base cancels and no choice of base is being
made. \\H_i\\ lies in \\\[0,1\]\\ and is exactly one when the neighbour
degrees are all equal, which is the source's \\1\le E_i\le 2\\.

The measure takes no parameters. Equation (6) is a bare product; the
exponents \\\alpha\\ and \\\beta\\ the Centrality Zoo attributes to Chen
et al. appear nowhere in the paper, and none is offered here.

**Two degenerate cases are cograph decisions, not the source's.** A node
with one neighbour has \\p=1\\, so its entropy is zero, and its
normaliser \\\log k_i\\ is zero too: \\H_i\\ is \\0/0\\ and is written
as **zero**, giving \\E_i=1\\. That is the value approached from
\\k_i=2\\ as one neighbour's share vanishes, and the one that gives a
node with a single path the least path diversity; the alternative
reading of \\0/0\\ as "the entropy equals its own maximum, so \\H=1\\"
would double every leaf's score. An isolate has both sums empty; \\H_i\\
is written as zero there as well, and the score is zero whatever finite
\\E_i\\ is chosen, because \\k_i\\ multiplies the product. Empty graphs
return no scores.

**Raw scores are not comparable across graphs of different order.**
\\N\\ in \\D_i\\ is the vertex count of the whole network, as the
source's own table 1 defines it, so adding a disconnected component – an
isolate included – changes every score, and unlike a plain rescaling it
can also change the ranking, because \\\exp(K_i/N)\\ shrinks the large
\\K_i\\ more than the small.

**The source's stated range \\1\le D_i\le e\\ is not general.** It holds
exactly when \\K_i\le N\\, which is true of the sparse toy networks of
its figure 1 and false on dense graphs: every node of \\K_5\\ has
\\K_i=16\\ against \\N=5\\, so \\D_i=e^{3.2}\\. cograph implements the
formula, not the range claim. Scores can therefore be large; \\K_i/N\le
(n-1)^2/n\\, so nothing overflows below about 710 vertices even on a
complete graph, and an overflow beyond that raises an error rather than
returning `Inf`.

**This is not the Centrality Zoo's formula.** Zoo section 2.215 writes
\\c\_{KED}(i)=k_i E_i^\alpha D_i^\beta\\ with
\\E_i=\bigl(\sum\_{j}-p_j\log p_j\bigr)/\log k_i\\ and
\\D_i=\exp(K_i/\max_l K_l)\\: it drops the \\1+\\ from \\E_i\\ and
divides by the largest cluster degree instead of by \\N\\. On the
source's own figure 1 that reading gives 13.5914 and 6.5672 where the
paper prints 25.9187 and 19.2212, which cograph reproduces. The \\\max_l
K_l\\ denominator is a plausible misreading, since it makes the paper's
stated \\1\le D_i\le e\\ hold, but it reproduces neither printed number.
cograph implements the paper and offers no Zoo variant.

Uses the simple undirected unweighted skeleton, the source's undirected
domain: either arc creates one edge, parallel edges count once and loops
are removed. Edge weights, mode, cutoff and path-weight inversion are
ignored. The source also defines a directed variant (its equation 3,
replacing the neighbourhood by the out-neighbourhood and \\k_i\\ by
\\k_i^{out}\\); that variant is not implemented, so a directed input is
symmetrised rather than being read as the paper's directed case. The
source states no normalization; `normalized = TRUE` max-scales the
finished vector as elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
Cost is two sparse matrix-vector products, O(n + m).

Numerical verification establishes agreement with the two scores the
source prints for its figure 1, not parity with author software, which
does not exist, and not any claim about spreading performance.

## References

Chen, D.-B., Xiao, R., Zeng, A. and Zhang, Y.-C. (2014). Path diversity
improves the identification of influential spreaders. Europhysics
Letters, 104(6), 68006. Equations (1) and (2) on page 2 and equation (6)
with its \\D_i\\ definition on page 4, read as the author preprint
arXiv:1305.7480.
[doi:10.1209/0295-5075/104/68006](https://doi.org/10.1209/0295-5075/104/68006)
.

## See also

[`centrality_lnc`](https://sonsoles.me/cograph/reference/centrality_lnc.md)
and
[`centrality_neighbor_distance`](https://sonsoles.me/cograph/reference/centrality_neighbor_distance.md)
for other neighbour-degree sums,
[`centrality_entropy`](https://sonsoles.me/cograph/reference/centrality_entropy.md)
for a plain neighbourhood entropy, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# Every node of a ring has two neighbours of degree two, so the
# neighbour degrees are even, H is one and the score is 4 exp(4 / n)
centrality_ked(igraph::make_ring(8))
#>        1        2        3        4        5        6        7        8 
#> 6.594885 6.594885 6.594885 6.594885 6.594885 6.594885 6.594885 6.594885 

# A star: the centre's neighbours are all leaves, so H is one again,
# and the centre scores exactly 2q times a leaf
centrality_ked(igraph::make_star(6, mode = "undirected"))
#>         1         2         3         4         5         6 
#> 23.009759  2.300976  2.300976  2.300976  2.300976  2.300976 
```
