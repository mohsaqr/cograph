# ArticleRank

ArticleRank (Li and Willett 2009) is a PageRank-style score for citation
networks in which a citing node passes on its score in a share set by
its number of references: \$\$AR(A) = 1 - d + d\\\overline{NR} \sum\_{P
\to A} \frac{AR(P)}{\overline{NR} + NR(P)},\$\$ with \\P \to A\\ when
\\P\\ cites \\A\\, \\NR(P)\\ the number of references of \\P\\,
\\\overline{NR}\\ its mean over the nodes and \\d\\ the damping factor.

## Usage

``` r
centrality_articlerank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ar_references` (the number of references of each
  node, default the out-degree) and `damping` (default 0.85).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the direction of the arcs and ignores weights; an
undirected tie is an arc each way, and loops are ignored. This is
equation (2) of the source, with the mean number of references as a
multiplier; the Centrality Zoo prints the form without it, which the
source's printed numbers rule out. The source counts all the references
of a paper, also those outside the network, so `ar_references` takes one
count per node (in node order, or named by node); `NULL`, the default,
takes the out-degree (owner decision 2026-11-26). `damping` is \\d\\,
0.85 by default as in the source. The scores are not normalised; a node
nobody cites scores \\1 - d\\. The source iterates the equation from
\\1 - d\\ to a precision of \\10^{-10}\\; that iteration converges, to
the solution of the linear system computed here, exactly when the
spectral radius of its linear map is below 1. A citation network without
cycles has radius 0. A cyclic network can have radius 1 or more, an
undirected one as soon as its degrees are large (an undirected network
where every node has degree \\k\\ and the references are the degrees has
radius \\dk/2\\); the score then has no value, and the measure warns
(class `cograph_undefined_measure`) and returns `NA` rather than
switching to another form. The implementation reproduces the 13 terms
\\AR(P)/(\overline{NR} + NR(P))\\ of the source's Table 5 whose citing
paper is not cited itself, and, on a citation network built so that the
46 citing papers of Table 5 have their printed scores, the printed
difference between the scores of papers P004 and P005 (0.49716); with
the same citing scores, the form without the multiplier gives a
difference 35.6 times smaller. On the four undirected test networks of
the other measures' screens the score has no value; oriented from lower
to higher node number, the four have Kendall correlations of at most
0.24 in absolute value between this score and any other measure. An
`ar_references` that is not one finite nonnegative number per node, or
whose names do not match the nodes, raises a `cograph_bad_parameter`
error, as do references that are all zero on a network with arcs.
`normalized = TRUE` divides the scores by their maximum.

## References

Li, J. and Willett, P. (2009). ArticleRank: a PageRank-based alternative
to numbers of citations for analysing citation networks. Aslib
Proceedings, 61(6), 605-618.
[doi:10.1108/00012530911005544](https://doi.org/10.1108/00012530911005544)
.

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
# Four papers: B, C and D cite A, and D cites B.
citations <- matrix(c(0, 0, 0, 0,  1, 0, 0, 0,  1, 0, 0, 0,  1, 1, 0, 0), 4, byrow = TRUE,
                    dimnames = list(LETTERS[1:4], LETTERS[1:4]))
centrality_articlerank(citations)
#>         A         B         C         D 
#> 0.3380625 0.1925000 0.1500000 0.1500000 
centrality_articlerank(citations, ar_references = c(A = 12, B = 30, C = 8, D = 45))
#>         A         B         C         D 
#> 0.3622993 0.1940455 0.1500000 0.1500000 
centrality_articlerank(regulation_net, damping = 0.5)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.335585   1.209805   2.729098   2.056152   2.045531   1.240876   1.014038 
#>   Evaluate     Create      Share 
#>   1.124507   1.855787   1.825183 
```
