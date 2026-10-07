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

  Accepted for a uniform interface. It has no effect. Default `"all"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ holds the edge weights with the diagonal set to zero. Edge weights
are always used, and `weighted = FALSE` has no effect. The `alpha`
argument of
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) is
the weight-inversion exponent, which this measure does not read. The
scores are positive when the spectral radius of \\A\\ is below one and
can be negative otherwise. A singular system or a negative edge weight
raises an error of class `cograph_singular_system`.

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
#>   4.226004   1.997912   5.811274   4.935278   4.717213   3.477159   1.838997 
#>   Evaluate     Create      Share 
#>   3.553234   4.036556   3.788677 
```
