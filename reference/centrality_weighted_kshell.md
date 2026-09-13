# Weighted k-shell, Renewed Coreness and Geodesic k-path

- `weighted_kshell` (Garas, Schweitzer & Havlin 2012):

  k-shell decomposition on the generalized degree \\k' = (k^\alpha
  s^\beta)^{1 / (\alpha + \beta)}\\ (`wks_alpha`, `wks_beta`, both 1),
  after the paper's weight normalization (divide by the mean, then by
  the minimum, round to the nearest integer). Integer thresholds label
  the shells, so unit weights give the k-core number and isolates
  score 0. Reproduces the paper's Figure 1 example and its Table 2 core
  size on the netscience network. Uses edge weights.

- `renewed_coreness` (Liu, Tang, Zhou & Do 2015):

  Each link gets the diffusion importance \\D\_{ij} = (\|N(j) \setminus
  N\[i\]\| + \|N(i) \setminus N\[j\]\|) / 2\\; links below
  `renewed_threshold` (paper: 2) are removed and the k-core number of
  the residual graph is the renewed coreness. A clique with no outside
  links collapses to 0. Reproduces the paper's Figure 1 and all twelve
  percentages of its supplementary Table S1; the Zoo's transcription
  with open neighborhoods is off by one.

- `geodesic_kpath` (Borgatti & Everett 2006):

  The number of shortest paths of length at most `kpath_k` (default 3)
  that start at the node, counted with multiplicity. Note that
  [`centiserve::geokpath`](https://rdrr.io/pkg/centiserve/man/geokpath.html)
  counts nodes within \\k\\ instead, which is the paper's
  vertex-disjoint variant and equals m-reach.

`geodesic_kpath` follows `mode`; the other two ignore direction.

## Usage

``` r
centrality_weighted_kshell(x, wks_alpha = 1, wks_beta = 1, ...)

centrality_renewed_coreness(x, renewed_threshold = 2, ...)

centrality_geodesic_kpath(x, mode = "all", kpath_k = 3, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- wks_alpha, wks_beta:

  Exponents of degree and strength in the weighted k-shell. Default 1
  and 1.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- renewed_threshold:

  Diffusion-importance threshold. Default 2.

- mode:

  For directed networks: `"all"` (default), `"out"` (distances along
  out-edges), or `"in"`.

- kpath_k:

  Maximum path length. Default 3.

## Value

Named numeric vector, one value per node.

## References

Garas, A., Schweitzer, F., & Havlin, S. (2012). A k-shell decomposition
method for weighted networks. New Journal of Physics, 14, 083030.

Liu, Y., Tang, M., Zhou, T., & Do, Y. (2015). Improving the accuracy of
the k-shell method by removing redundant links. Scientific Reports, 5,
13172.

Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
on centrality. Social Networks, 28(4), 466-484.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_s_shell`](https://sonsoles.me/cograph/reference/centrality_s_shell.md),
[`centrality_kreach`](https://sonsoles.me/cograph/reference/centrality_kreach.md).

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_weighted_kshell(adj)
#> A B C D E F 
#> 2 2 2 2 2 2 
centrality_renewed_coreness(adj)
#> A B C D E F 
#> 0 0 1 1 0 0 
centrality_geodesic_kpath(adj, kpath_k = 2)
#> A B C D E F 
#> 3 3 5 5 3 3 
```
