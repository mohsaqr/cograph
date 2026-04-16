# Katz Centrality

Katz (1953) status index: \\C = (I - \alpha A^T)^{-1} \mathbf{1}\\. Each
node's score sums attenuated walks of every length back to it, with
attenuation \\\alpha\\ applied per step. Rankings are identical to
Bonacich's alpha centrality with a uniform exogenous vector.

## Usage

``` r
centrality_katz(x, katz_alpha = 0.1, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- katz_alpha:

  Attenuation factor. Must satisfy \\\alpha \< 1 / \rho(A)\\ where
  \\\rho(A)\\ is the spectral radius. Default 0.1 matches centiserve and
  NetworkX conventions.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of Katz centrality values.

## Details

Equivalence is verified bit-exact against
[`centiserve::katzcent`](https://rdrr.io/pkg/centiserve/man/katzcent.html)
(cograph mirrors centiserve's exact LAPACK call sequence) and at machine
epsilon against `igraph::alpha_centrality(exo = 1)` and
`networkx.katz_centrality_numpy`.

## References

Katz, L. (1953). A new status index derived from sociometric analysis.
*Psychometrika*, 18(1), 39-43.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md).

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_katz(adj)
#>    A    B    C 
#> 1.25 1.25 1.25 
```
