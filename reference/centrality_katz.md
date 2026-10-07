# Katz Centrality

Katz (1953) status sums the walks of every length that end at a node,
each step attenuated by \\\alpha\\: \$\$c = (I - \alpha
A^{T})^{-1}\mathbf{1},\$\$ where \\A\\ is the weighted adjacency matrix
and \\\alpha\\ is `katz_alpha`.

## Usage

``` r
centrality_katz(x, katz_alpha = 0.1, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- katz_alpha:

  Attenuation factor \\\alpha\\ (default 0.1).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The series converges for \\\alpha \< 1/\rho(A)\\, where \\\rho(A)\\ is
the spectral radius. A divergent series is detected from scores below
one and raises a `cograph_katz_diverged` warning that names the bound;
the returned values are then not Katz scores. `weighted = FALSE` gives
every edge weight one. On a directed network the score counts walks that
arrive at the node. A network of one node without a self-loop scores 1.
The values equal `igraph::alpha_centrality(exo = 1)` with the same
`alpha`.

## References

Katz, L. (1953). A new status index derived from sociometric analysis.
*Psychometrika*, 18(1), 39-43.

## See also

[`centrality_alpha`](https://sonsoles.me/cograph/reference/centrality_alpha.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_hubbell`](https://sonsoles.me/cograph/reference/centrality_hubbell.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_katz(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.084116   1.034124   1.145330   1.107873   1.126150   1.079033   1.018834 
#>   Evaluate     Create      Share 
#>   1.092721   1.078166   1.091297 
```
