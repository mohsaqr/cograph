# SpectralRank Centrality

SpectralRank (Xu et al. 2019) is the positive eigenvector of the largest
eigenvalue of the adjacency matrix augmented by a ground node, which is
linked in both directions to every node with unit weight: \$\$B =
\left(\begin{smallmatrix} A + P & \mathbf{1} \\ \mathbf{1}^T & 0
\end{smallmatrix}\right).\$\$ The diagonal prior \\P\\ is zero for
ordinary SpectralRank, and a nonnegative prior gives the weighted
SpectralRank family of the paper.

## Usage

``` r
centrality_spectralrank(x, sr_prior = 0, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- sr_prior:

  Diagonal prior \\P\\, a nonnegative scalar or one value per node. A
  named vector is matched to the node names. Default 0, which gives
  ordinary SpectralRank.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (use edge weights, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The eigenvector is divided by its maximum over all nodes including the
ground node, which is then dropped, so the largest returned score can be
below one. `normalized = TRUE` further divides by the maximum over the
original nodes. An arc from \\i\\ to \\j\\ contributes the score of
\\j\\ to \\i\\. Edge weights must be finite and nonnegative,
`weighted = FALSE` gives every edge weight one, and loops are removed.
Every node, isolated nodes included, scores positive, and without edges
or priors each of \\n\\ nodes scores \\1/\sqrt{n}\\. The update line of
Algorithm 1 in the paper omits \\P\\, and the implementation follows
section III-A2 with \\B = \tilde{A} + P\\. An invalid `sr_prior` or an
unresolved eigenvector raises an error.

## References

Xu, S., Wang, P., Zhang, C.-X. and Lu, J. (2019). Spectral Learning
Algorithm Reveals Propagation Capability of Complex Networks. IEEE
Transactions on Cybernetics, 49(12), 4253-4261.
[doi:10.1109/TCYB.2018.2861568](https://doi.org/10.1109/TCYB.2018.2861568)
.

## See also

[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_leaderrank`](https://sonsoles.me/cograph/reference/centrality_leaderrank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_spectralrank(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3383201  0.4405105  0.3341652  0.3555215  0.2984056  0.3518547  0.3347994 
#>   Evaluate     Create      Share 
#>  0.3590902  0.3730458  0.3900942 
```
