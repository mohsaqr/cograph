# ControlRank Centrality

ControlRank (Zhou, Yu and Lu 2019) is the smallest eigenvalue of the
symmetric part of the graph Laplacian \\L = D - A\\ after the row and
column of the node are deleted: \$\$CR_i =
\lambda\_{\min}\left(\left(\frac{L + L^T}{2}\right)\_{-i,-i}
\right).\$\$ Larger values rank higher.

## Usage

``` r
centrality_controlrank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`) and
  `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights must be finite and nonnegative, `weighted = FALSE` gives
every edge weight one, and loops are removed. In a directed network
\\A\_{ij}\\ is the arc from \\i\\ to \\j\\, \\D\\ holds the
out-strengths and scores can be negative. On a connected undirected
network with at least two nodes all scores are positive, and on a
disconnected undirected network every score is zero. A single node
scores zero. With `normalized = TRUE` the scores are divided by their
maximum when it is positive, so negative directed scores stay negative.
For matrix input with very small weights, set `directed = TRUE`
explicitly, because symmetry detection is approximate. A weight range
beyond double precision or an unresolved spectrum raises an error.

## References

Zhou, J., Yu, X. and Lu, J.-A. (2019). Node Importance in Controlled
Complex Networks. IEEE Transactions on Circuits and Systems II: Express
Briefs, 66(3), 437-441.
[doi:10.1109/TCSII.2018.2845940](https://doi.org/10.1109/TCSII.2018.2845940)
.

## See also

[`centrality_laplacian`](https://sonsoles.me/cograph/reference/centrality_laplacian.md),
[`centrality_spectralrank`](https://sonsoles.me/cograph/reference/centrality_spectralrank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_controlrank(regulation_net)
#>     Explore        Plan     Monitor       Adapt     Reflect     Discuss 
#> -0.02646079 -0.08739707 -0.04912661 -0.06026944  0.03788145 -0.04541166 
#>  Synthesize    Evaluate      Create       Share 
#> -0.07026868 -0.07521652 -0.07620639 -0.07268062 
```
