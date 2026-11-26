# NWRank

NWRank (Wang et al. 2015) ranks nodes and links together: a node passes
its score to its links, and a link passes its score to its two end
nodes. Node \\j\\ gives link \\b\\ the share \$\$h\_{bj} = \alpha
\frac{k_b}{\sum_a k_a} + (1 - \alpha) \frac{BC_b}{\sum_a BC_a},\$\$
where the sums run over the links at \\j\\, \\k_b\\ is the degree of the
node at the other end of \\b\\ and \\BC_b\\ is the betweenness of the
link. The node scores are the principal eigenvector of \\WH\\, with
\\W\\ the node-link incidence matrix, scaled to sum to one.

## Usage

``` r
centrality_nwrank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `nwrank_alpha` (the weight of the degree term,
  default 0.5).

## Value

A named numeric vector with one score per node, in input node order,
summing to one, or `NA` at every node when the score is undefined.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`nwrank_alpha` is 0.5 by default, the value of the source's case study.
At 1 only the degrees decide the shares and at 0 only the link
betweenness. Isolated nodes score 0. When the nodes that have links form
more than one component the eigenvector is not unique and every score is
`NA` with a `cograph_undefined_measure` warning; the same holds for a
network without links. A `nwrank_alpha` outside \\\[0,1\]\\ raises a
`cograph_bad_parameter` error.

## References

Wang, Z., Duenas-Osorio, L. and Padgett, J. E. (2015). A new mutually
reinforcing network node and link ranking algorithm. Scientific Reports,
5, 15141. [doi:10.1038/srep15141](https://doi.org/10.1038/srep15141) .

## See also

[`centrality_linerank`](https://sonsoles.me/cograph/reference/centrality_linerank.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_nwrank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.08820520 0.11281176 0.13870985 0.11620242 0.09278267 0.09008064 0.07031504 
#>   Evaluate     Create      Share 
#> 0.09000918 0.11143014 0.08945308 
```
