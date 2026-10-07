# Alpha Centrality

Alpha centrality (Bonacich and Lloyd 2001) gives every node an exogenous
score of one plus the scores of the nodes that point to it: \$\$x =
\alpha A^{T} x + e, \qquad x = (I - \alpha A^{T})^{-1} e.\$\$ The
attenuation is fixed at \\\alpha = 1\\ and \\e\\ is a vector of ones,
the defaults of
[`igraph::alpha_centrality()`](https://r.igraph.org/reference/alpha_centrality.html).

## Usage

``` r
centrality_alpha(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"in"` or `"out"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ holds the edge weights, or ones with `weighted = FALSE`, with the
diagonal set to zero. The `alpha` argument of
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) is
the weight-inversion exponent, which this measure does not read. The
scores are positive when the spectral radius of \\A\\ is below one and
can be negative otherwise. A singular system or a negative edge weight
raises an error of class `cograph_singular_system`.

On a directed network `mode = "in"` sums over incoming ties as above and
equals
[`igraph::alpha_centrality()`](https://r.igraph.org/reference/alpha_centrality.html),
`mode = "out"` uses \\A\\ in place of \\A^{T}\\ and so sums over
outgoing ties, and `mode = "all"` (default) uses the symmetrized weights
\\A + A^{T}\\. On an undirected network the three modes agree.

## References

Bonacich, P., & Lloyd, P. (2001). Eigenvector-like measures of
centrality for asymmetric relations. Social Networks, 23(3), 191-201.
[doi:10.1016/S0378-8733(01)00038-7](https://doi.org/10.1016/S0378-8733%2801%2900038-7)
.

## See also

[`centrality_katz`](https://sonsoles.me/cograph/reference/centrality_katz.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_alpha(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> -0.1294030 -2.1294525 -2.2733148 -1.2166591  0.6012820 -0.3834963  0.6523346 
#>   Evaluate     Create      Share 
#> -2.0816623 -2.0691360 -2.3130493 
```
