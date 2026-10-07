# Eigenvector Centrality

Eigenvector centrality (Bonacich 1972) scores a node by the scores of
the nodes that point to it. The scores form the dominant eigenvector of
the transposed weight matrix: \$\$\lambda x = A^{T} x.\$\$ The vector is
scaled to a maximum of one.

## Usage

``` r
centrality_eigenvector(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ holds the edge weights, or ones with `weighted = FALSE`. On a
directed network a node gains standing from its incoming edges. The
scores lie between 0 and 1. A network without edges gives every node a
score of one.

## References

Bonacich, P. (1972). Factoring and weighting approaches to status scores
and clique identification. Journal of Mathematical Sociology, 2(1),
113-120.
[doi:10.1080/0022250X.1972.9989806](https://doi.org/10.1080/0022250X.1972.9989806)
.

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality_alpha`](https://sonsoles.me/cograph/reference/centrality_alpha.md),
[`centrality_authority`](https://sonsoles.me/cograph/reference/centrality_authority.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_eigenvector(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7209674  0.2010163  1.0000000  0.8380103  0.7619198  0.5077694  0.1980065 
#>   Evaluate     Create      Share 
#>  0.4995074  0.6689427  0.5849834 
```
